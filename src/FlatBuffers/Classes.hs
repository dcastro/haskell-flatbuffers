{-# LANGUAGE TypeApplications #-}

module FlatBuffers.Classes where

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as G
import           Data.Int
import           Data.Word

class PrimField f where
  dflt :: f
  getter :: Get f


instance PrimField Int8 where
  dflt = 0
  getter = G.getInt8

instance PrimField Int16 where
  dflt = 0
  getter = G.getInt16le

instance PrimField Int32 where
  dflt = 0
  getter = G.getInt32le

instance PrimField Int64 where
  dflt = 0
  getter = G.getInt64le


instance PrimField Word8 where
  dflt = 0
  getter = G.getWord8

instance PrimField Word16 where
  dflt = 0
  getter = G.getWord16le

instance PrimField Word32 where
  dflt = 0
  getter = G.getWord32le

instance PrimField Word64 where
  dflt = 0
  getter = G.getWord64le

instance PrimField Bool where
  dflt = False
  getter = toBool <$> getter @Word8
    where
      toBool 0 = False
      toBool _ = True

instance PrimField Float where
  dflt = 0
  getter = G.getFloatle

instance PrimField Double where
  dflt = 0
  getter = G.getDoublele
