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
import           Data.Int
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import           Data.Word
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
  { fbRoot      :: !ByteString
  , table       :: !ByteString
  , vtable      :: !ByteString
  , tableOffset :: !OffsetFromRoot
  }

fromLazyByteString :: ReadCtx m => ByteString -> m Table
fromLazyByteString root = runGetM (getTable root 0) root

getTable :: ByteString -> OffsetFromRoot -> Get Table
getTable root currentOffsetFromRoot = do
  tableOffset <- G.getWord32le
  G.skip (fromIntegral @Word32 @Int tableOffset - 4)
  soffset <- G.getInt32le

  let tableOffset64 = fromIntegral @Word32 @Int64 tableOffset
  let vtable = BSL.drop (tableOffset64 - widen64 soffset + fromIntegral @_ @Int64 currentOffsetFromRoot) root
  let table = BSL.drop (tableOffset64 + fromIntegral @_ @Int64 currentOffsetFromRoot) root
  pure $ Table root table vtable (OffsetFromRoot $ widen64 tableOffset)

readInt32 :: ReadCtx m => Table -> Index -> Int32 -> m Int32
readInt32 t ix dflt = indexToVOffset t ix >>= readFromVOffset t G.getInt32le (pure dflt)

readInt32Req :: ReadCtx m => Table -> Index -> FieldName -> m Int32
readInt32Req t ix fn = indexToVOffset t ix >>= readFromVOffset t G.getInt32le (throwM $ MissingField fn)

readInt64 :: ReadCtx m => Table -> Index -> Int64 -> m Int64
readInt64 t ix dflt = indexToVOffset t ix >>= readFromVOffset t G.getInt64le (pure dflt)

readTableReq :: ReadCtx m => Table -> Index -> FieldName -> m Table
readTableReq t ix fn = do
  voffset <- indexToVOffset t ix
  if voffset == 0
    then throwM $ MissingField fn
    else
      flip runGetM (table t) $ do
        G.skip (fromIntegral @_ @Int voffset)
        getTable
          (fbRoot t)
          (tableOffset t + fromIntegral @VOffset @OffsetFromRoot voffset)

readFromVOffset :: ReadCtx m => Table -> Get a -> m a -> VOffset -> m a
readFromVOffset _ _ dflt 0 = dflt
readFromVOffset t get _ voffset = runGetM (G.skip (fromIntegral @_ @Int voffset) >> get) (table t)

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
        