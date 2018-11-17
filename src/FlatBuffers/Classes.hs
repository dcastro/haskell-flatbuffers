module FlatBuffers.Classes where

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as G
import           Data.Int
import           Data.Word

class NumericField f where
  dflt :: f
  getter :: Get f


instance NumericField Int8 where
  dflt = 0
  getter = G.getInt8

instance NumericField Int16 where
  dflt = 0
  getter = G.getInt16le

instance NumericField Int32 where
  dflt = 0
  getter = G.getInt32le

instance NumericField Int64 where
  dflt = 0
  getter = G.getInt64le


instance NumericField Word8 where
  dflt = 0
  getter = G.getWord8

instance NumericField Word16 where
  dflt = 0
  getter = G.getWord16le

instance NumericField Word32 where
  dflt = 0
  getter = G.getWord32le

instance NumericField Word64 where
  dflt = 0
  getter = G.getWord64le
  