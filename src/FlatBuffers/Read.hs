{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlatBuffers.Read where

import qualified Data.Binary.Get             as BG
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Int
import           Data.Word
import           FlatBuffers                 (int32, root, scalar)
import           HaskellWorks.Data.Int.Widen (widen16, widen32, widen64)

newtype Index = Index { unIndex :: Word16 }
  deriving Num

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving (Num, Eq)

data Table = Table
  { table  :: !ByteString
  , vtable :: !ByteString
  }

rootAsTable :: ByteString -> Table
rootAsTable bs = BG.runGet table bs
  where
    table = do
      offset <- widen64 <$> BG.getWord32le
      BG.skip (fromIntegral offset - 4)
      voffset <- BG.getInt32le
      let vtable = BSL.drop (fromIntegral offset - widen64 voffset) bs
      let table = BSL.drop (fromIntegral offset) bs
      pure $ Table {..}

readInt32 :: Table -> Index -> Int32 -> Int32
readInt32 t ix dflt = readFromOffset t BG.getInt32le dflt $ fieldOffset t ix

readInt64 :: Table -> Index -> Int64 -> Int64
readInt64 t ix dflt = readFromOffset t BG.getInt64le dflt $ fieldOffset t ix

readFromOffset :: Table -> BG.Get a -> a -> VOffset -> a
readFromOffset _ _ dflt 0 = dflt
readFromOffset t get _ voffset = BG.runGet (BG.skip (fromIntegral $ unVOffset voffset) >> get) (table t)

fieldOffset :: Table -> Index -> VOffset
fieldOffset Table {..} ix =
  let vtableSize = BG.runGet BG.getWord16le vtable
      vtableIndex = 4 + (unIndex ix * 2)
      voffset =
        if vtableIndex < vtableSize
          then BG.runGet
                  (BG.skip (fromIntegral vtableIndex) >> BG.getWord16le)
                  vtable
          else 0
  in VOffset voffset
