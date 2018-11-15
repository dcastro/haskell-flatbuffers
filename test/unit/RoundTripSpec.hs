{-# LANGUAGE OverloadedStrings #-}

module RoundTripSpec where

import qualified Data.ByteString.Builder as B
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Int
import           Data.Tagged             (Tagged (..), untag)
import           FlatBuffers
import qualified FlatBuffers             as F
import           FlatBuffers.Read
import           Test.Hspec

spec :: Spec
spec =
  describe "Simple RoundTrip" $ do
    it "should encode/decode inline table fields" $ do
      let bs = encodeSimple 99 maxBound (encodeNested 123)
      s <- simpleFromLazyByteString bs

      simpleA s `shouldBe` Just 99
      simpleB s `shouldBe` Just maxBound

    it "should encode/decode nested tables" $ do
      let bs = encodeSimple 99 maxBound (encodeNested 123)
      s <- simpleFromLazyByteString bs

      nested <- simpleC s
      nestedA nested `shouldBe` Just 123

newtype Simple =
  Simple Table

simpleFromLazyByteString :: ReadCtx m => ByteString -> m Simple
simpleFromLazyByteString bs = Simple <$> fromLazyByteString bs

encodeSimple :: Int32 -> Int64 -> Tagged Nested Field -> ByteString
encodeSimple a b c =
  B.toLazyByteString $
  root
    [ scalar int32 a
    , scalar int64 b
    , untag c
    ]

simpleA :: ReadCtx m => Simple -> m Int32
simpleA (Simple t) = readInt32 t 0 0

simpleB :: ReadCtx m => Simple -> m Int64
simpleB (Simple t) = readInt64 t 1 0

simpleC :: ReadCtx m => Simple -> m Nested
simpleC (Simple t) = Nested <$> readTableReq t 2 "c"


newtype Nested =
  Nested Table

nestedFromLazyByteString :: ReadCtx m => ByteString -> m Nested
nestedFromLazyByteString bs = Nested <$> fromLazyByteString bs

encodeNested :: Int32 -> Tagged Nested Field
encodeNested a =
  Tagged $ F.table
    [ scalar int32 a ]

nestedA :: ReadCtx m => Nested -> m Int32
nestedA (Nested t) = readInt32 t 0 0
