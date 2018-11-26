{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
import           Data.Functor                  ((<&>))
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
  { vtable   :: !ByteString
  , tablePos :: !Position
  }

newtype Struct = Struct { unStruct :: Position }

-- | Current position in the buffer
data Position = Position
  { posRoot           :: !ByteString -- ^ Pointer to the buffer root
  , posCurrent        :: !ByteString -- ^ Pointer to current position
  , posOffsetFromRoot :: !OffsetFromRoot -- ^ Number of bytes between current position and root
  }

class HasPosition a where
  getPos :: a -> Position

instance HasPosition Position where getPos = id
instance HasPosition Table    where getPos = tablePos
instance HasPosition Struct   where getPos = unStruct

class HasTable a where
  getTable :: a -> Table

instance HasTable Table where getTable = id



tableFromLazyByteString :: ReadCtx m => ByteString -> m Table
tableFromLazyByteString root = readTable initialPos
  where
    initialPos = Position root root 0

move :: HasPosition p => p -> VOffset -> Position
move hs offset =
  Position
  { posRoot = posRoot
  , posCurrent = BSL.drop (fromIntegral @VOffset @Int64 offset) posCurrent
  , posOffsetFromRoot = posOffsetFromRoot + fromIntegral @VOffset @OffsetFromRoot offset
  }
  where Position{..} = getPos hs

readNumerical :: (ReadCtx m, NumericField f) => Position -> m f 
readNumerical Position{..} = runGetM getter posCurrent

readText :: ReadCtx m => Position -> m Text
readText Position{..} = do
  bs <- flip runGetM posCurrent $ do
    moveUOffset
    strLength <- G.getWord32le
    G.getByteString $ fromIntegral @Word32 @Int strLength
  case T.decodeUtf8' bs of
    Right t -> pure t
    Left (T.DecodeError msg b) -> throwM $ Utf8DecodingError msg b
    -- The `EncodeError` constructor is deprecated and not used
    -- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Encoding-Error.html#t:UnicodeException
    Left _ -> error "the impossible happened"

readStruct :: Position -> Struct
readStruct = Struct

readTable :: ReadCtx m => Position -> m Table
readTable Position{..} =
  flip runGetM posCurrent $ do
    tableOffset <- moveUOffset
    soffset <- G.getInt32le

    let tableOffset64 = fromIntegral @Word32 @Int64 tableOffset
    let tableOffsetFromRoot = tableOffset64 + fromIntegral @_ @Int64 posOffsetFromRoot
    let vtable = BSL.drop (tableOffsetFromRoot - widen64 soffset) posRoot
    let table = BSL.drop tableOffsetFromRoot posRoot
    pure $ Table vtable (Position posRoot table (posOffsetFromRoot + OffsetFromRoot (widen64 tableOffset)))

required :: ReadCtx m => FieldName -> (VOffset -> m a) -> Maybe VOffset -> m a
required _ f (Just vo) = f vo
required fn _ _ = throwM $ MissingField fn

optional :: ReadCtx m => a -> (VOffset -> m a) -> Maybe VOffset -> m a
optional _ f (Just vo) = f vo
optional dflt _ _ = pure dflt

tableIndexToVOffset :: (ReadCtx m, HasTable t) => t -> Index -> m (Maybe VOffset)
tableIndexToVOffset a ix =
  flip runGetM vtable $ do
    vtableSize <- G.getWord16le
    let vtableIndex = 4 + (unIndex ix * 2)
    if vtableIndex >= vtableSize
      then pure Nothing
      else do
        G.skip (fromIntegral @Word16 @Int vtableIndex - 2)
        G.getWord16le <&> \case
          0 -> Nothing
          word16 -> Just (VOffset word16)
  where Table{..} = getTable a

moveUOffset :: Get Word32
moveUOffset = do
  uoffset <- G.getWord32le
  G.skip (fromIntegral @Word32 @Int uoffset - 4)
  pure uoffset


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
        