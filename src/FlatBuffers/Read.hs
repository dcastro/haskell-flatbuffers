{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module FlatBuffers.Read where
  
import           Control.Exception.Safe        (Exception, MonadThrow, throwM)
import           Data.Binary.Get               (Get)
import qualified Data.Binary.Get               as G
import qualified Data.ByteString               as BS
import           Data.ByteString.Lazy          (ByteString)
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.Coerce                   (coerce)
import           Data.Int
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import           Data.Word
import           FlatBuffers.Classes           (NumericField (..))
import           HaskellWorks.Data.Int.Widen   (widen16, widen32, widen64)


type ReadCtx m = MonadThrow m

newtype FieldName = FieldName Text
  deriving (Show, Eq, IsString)

newtype Index = Index { unIndex :: Word16 }
  deriving (Show, Num)

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving (Show, Num, Real, Ord, Enum, Integral, Eq)

newtype UOffset = UOffset { unUOffset :: Word32 }
  deriving (Show, Num, Eq)

newtype OffsetFromRoot = OffsetFromRoot { unOffsetFromRoot :: Word64 }
  deriving (Show, Num, Real, Ord, Enum, Integral, Eq)

data Table = Table
  { tableRoot   :: !ByteString
  , table       :: !ByteString
  , vtable      :: !ByteString
  , tableOffset :: !OffsetFromRoot
  }

data Struct = Struct
  { structRoot   :: !ByteString
  , struct       :: !ByteString
  , structOffset :: !OffsetFromRoot
  }

tableFromLazyByteString :: ReadCtx m => ByteString -> m Table
tableFromLazyByteString root = runGetM (getTable root 0) root

getTable :: ByteString -> OffsetFromRoot -> Get Table
getTable root currentOffsetFromRoot = do
  tableOffset <- G.getWord32le
  G.skip (fromIntegral @Word32 @Int tableOffset - 4)
  soffset <- G.getInt32le

  let tableOffset64 = fromIntegral @Word32 @Int64 tableOffset
  let vtable = BSL.drop (tableOffset64 - widen64 soffset + fromIntegral @_ @Int64 currentOffsetFromRoot) root
  let table = BSL.drop (tableOffset64 + fromIntegral @_ @Int64 currentOffsetFromRoot) root
  pure $ Table root table vtable (OffsetFromRoot $ widen64 tableOffset)

numericalFromIndex :: (ReadCtx m, NumericField f) => Table -> Index -> f -> m f
numericalFromIndex t ix dflt' = indexToVOffset t ix >>= readFromVOffset (table t) getter (pure dflt')

numericalFromVOffset :: (ReadCtx m, NumericField f) => BSL.ByteString -> VOffset -> m f
numericalFromVOffset bs voff = readFromVOffset' bs getter voff


textFromIndexReq :: ReadCtx m => Table -> Index -> FieldName -> m Text
textFromIndexReq t ix fn = do
  voffset <- indexToVOffset t ix
  if voffset == 0
    then throwM $ MissingField fn
    else do
      bs <- flip runGetM (table t) $ do
        G.skip (fromIntegral @VOffset @Int voffset)
        uoffset <- G.getWord32le
        G.skip (fromIntegral @Word32 @Int uoffset - 4)
        strLength <- G.getWord32le
        G.getByteString $ fromIntegral @Word32 @Int strLength
      case T.decodeUtf8' bs of
        Right t -> pure t
        Left (T.DecodeError msg b) -> throwM $ Utf8DecodingError msg b
        -- The `EncodeError` constructor is deprecated and not used
        -- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Encoding-Error.html#t:UnicodeException
        Left _ -> error "the impossible happened"

structFromIndexReq :: ReadCtx m => Table -> Index -> FieldName -> m Struct
structFromIndexReq t ix fn = do
  voffset <- indexToVOffset t ix
  if voffset == 0
    then throwM $ MissingField fn
    else pure
           Struct
           { structRoot = tableRoot t
           , struct = BSL.drop (fromIntegral @VOffset @Int64 voffset) (table t)
           , structOffset = tableOffset t + (coerce . widen64 . unVOffset $ voffset)
           }

tableFromIndexReq :: ReadCtx m => Table -> Index -> FieldName -> m Table
tableFromIndexReq t ix fn = do
  voffset <- indexToVOffset t ix
  if voffset == 0
    then throwM $ MissingField fn
    else
      flip runGetM (table t) $ do
        G.skip (fromIntegral @_ @Int voffset)
        getTable
          (tableRoot t)
          (tableOffset t + fromIntegral @VOffset @OffsetFromRoot voffset)

readFromVOffset' :: ReadCtx m => BSL.ByteString -> Get a -> VOffset -> m a
readFromVOffset' bs get voffset = runGetM (G.skip (fromIntegral @_ @Int voffset) >> get) bs

-- | Returns `dflt` is voffset is 0
readFromVOffset :: ReadCtx m => BSL.ByteString -> Get a -> m a -> VOffset -> m a
readFromVOffset _ _ dflt 0 = dflt
readFromVOffset bs get _ voffset = runGetM (G.skip (fromIntegral @_ @Int voffset) >> get) bs

indexToVOffset :: ReadCtx m => Table -> Index -> m VOffset
indexToVOffset Table {..} ix =
  flip runGetM vtable $ do
    vtableSize <- G.getWord16le
    let vtableIndex = 4 + (unIndex ix * 2)
    voffset <-
      if vtableIndex < vtableSize
        then G.skip (fromIntegral @Word16 @Int vtableIndex - 2) >> G.getWord16le
        else pure 0
    pure $ VOffset voffset

data Error
  = ParsingError { position :: G.ByteOffset
                 , msg      :: String }
  | MissingField { fieldName :: FieldName }
  | Utf8DecodingError { msg  :: String
                      , byte :: Maybe Word8 }
  deriving (Show, Eq)

instance Exception Error

runGetM :: ReadCtx m => Get a -> ByteString -> m a
runGetM get =
  feedAll (G.runGetIncremental get)
  where
    feedAll (G.Done _ _ x) _ = pure x
    feedAll (G.Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
    feedAll (G.Fail _ pos msg) _ = throwM $ ParsingError pos msg

    takeHeadChunk :: BSL.ByteString -> Maybe BS.ByteString
    takeHeadChunk lbs =
      case lbs of
        (BSL.Chunk bs _) -> Just bs
        _ -> Nothing

    dropHeadChunk :: BSL.ByteString -> BSL.ByteString
    dropHeadChunk lbs =
      case lbs of
        (BSL.Chunk _ lbs') -> lbs'
        _ -> BSL.Empty
        