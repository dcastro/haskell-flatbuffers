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
    s <- simpleFromLazyByteString bs
    
    simpleA s `shouldBe` Just 99
    simpleB s `shouldBe` Just minBound
    simpleC s `shouldBe` Just maxBound
    simpleD s `shouldBe` Just 12
    simpleE s `shouldBe` Just maxBound

newtype Simple =
  Simple Table

simpleFromLazyByteString :: ReadCtx m => ByteString -> m Simple
simpleFromLazyByteString bs = Simple <$> fromLazyByteString bs

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

simpleA :: ReadCtx m => Simple -> m Int32
simpleA (Simple t) = readInt32 t 0 0

simpleB :: ReadCtx m => Simple -> m Int32
simpleB (Simple t) = readInt32 t 1 0

simpleC :: ReadCtx m => Simple -> m Int32
simpleC (Simple t) = readInt32 t 2 0

simpleD :: ReadCtx m => Simple -> m Int64
simpleD (Simple t) = readInt64 t 3 0

simpleE :: ReadCtx m => Simple -> m Int64
simpleE (Simple t) = readInt64 t 4 0
