{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module FlatBuffers.Read where

import           Control.Exception.Safe        (Exception, MonadThrow, throwM)
import           Data.Binary.Get               (Get)
import qualified Data.Binary.Get               as G
import qualified Data.ByteString               as BS
import           Data.ByteString.Lazy          (ByteString)
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.Int
import           Data.Word
import           HaskellWorks.Data.Int.Widen   (widen16, widen32, widen64)

type ReadCtx m = MonadThrow m
 
newtype Index = Index { unIndex :: Word16 }
  deriving Num

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving (Num, Eq)

data Table = Table
  { table  :: !ByteString
  , vtable :: !ByteString
  }

fromLazyByteString :: ReadCtx m => ByteString -> m Table
fromLazyByteString bs = runGetM table bs
  where
    table = do
      offset <- widen64 <$> G.getWord32le
      G.skip (fromIntegral offset - 4)
      voffset <- G.getInt32le
      let vtable = BSL.drop (fromIntegral offset - widen64 voffset) bs
      let table = BSL.drop (fromIntegral offset) bs
      pure $ Table {..}

readInt32 :: ReadCtx m => Table -> Index -> Int32 -> m Int32
readInt32 t ix dflt = readFromVOffset t G.getInt32le dflt =<< fieldVOffset t ix

readInt64 :: ReadCtx m => Table -> Index -> Int64 -> m Int64
readInt64 t ix dflt = readFromVOffset t G.getInt64le dflt =<< fieldVOffset t ix


readFromVOffset :: ReadCtx m => Table -> Get a -> a -> VOffset -> m a
readFromVOffset _ _ dflt 0 = pure dflt
readFromVOffset t get _ voffset = runGetM (G.skip (fromIntegral $ unVOffset voffset) >> get) (table t)

fieldVOffset :: ReadCtx m => Table -> Index -> m VOffset
fieldVOffset Table {..} ix = do
  vtableSize <- runGetM G.getWord16le vtable
  let vtableIndex = 4 + (unIndex ix * 2)
  voffset <-
    if vtableIndex < vtableSize
      then runGetM (G.skip (fromIntegral vtableIndex) >> G.getWord16le) vtable
      else pure 0
  pure $ VOffset voffset


data ParsingError = ParsingError
  { position :: G.ByteOffset
  , msg      :: String
  }
  deriving (Show, Eq)

instance Exception ParsingError

runGetM :: ReadCtx m => Get a -> ByteString -> m a
runGetM get =
  feedAll (G.runGetIncremental get)
  where
    feedAll (G.Done bs pos x) lbs = pure x
    feedAll (G.Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
    feedAll (G.Fail x pos msg) xs = throwM $ ParsingError pos msg

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
        