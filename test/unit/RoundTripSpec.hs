module RoundTripSpec where

import qualified Data.ByteString.Builder as B
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Int
import           FlatBuffers
import           FlatBuffers.Read
import           Test.Hspec

spec :: Spec
spec =
  describe "Simple RoundTrip" $
  it "should encode/decode inline table fields" $ do
    let bs = encodeSimple 99 minBound maxBound 12 maxBound
    let s = rootAsSimple bs
    
    simpleA s `shouldBe` 99
    simpleB s `shouldBe` minBound
    simpleC s `shouldBe` maxBound
    simpleD s `shouldBe` 12
    simpleE s `shouldBe` maxBound

newtype Simple =
  Simple Table

rootAsSimple :: ByteString -> Simple
rootAsSimple = Simple . rootAsTable

encodeSimple :: Int32 -> Int32 -> Int32 -> Int64 -> Int64 -> ByteString
encodeSimple a b c d e =
  B.toLazyByteString $
  root
    [ scalar int32 a
    , scalar int32 b
    , scalar int32 c
    , scalar int64 d
    , scalar int64 e
    ]

simpleA :: Simple -> Int32
simpleA (Simple t) = readInt32 t 0 0

simpleB :: Simple -> Int32
simpleB (Simple t) = readInt32 t 1 0

simpleC :: Simple -> Int32
simpleC (Simple t) = readInt32 t 2 0

simpleD :: Simple -> Int64
simpleD (Simple t) = readInt64 t 3 0

simpleE :: Simple -> Int64
simpleE (Simple t) = readInt64 t 4 0
