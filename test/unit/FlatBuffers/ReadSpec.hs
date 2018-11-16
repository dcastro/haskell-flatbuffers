{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.ReadSpec where

import qualified Data.ByteString.Builder as B
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Int
import           Data.Tagged             (Tagged (..), untag)
import qualified FlatBuffers             as F
import           FlatBuffers.Dsl
import           FlatBuffers.Read
import           Test.Hspec

spec :: Spec
spec =
  describe "read" $ do 
    it "throws when buffer is exhausted" $
      fromLazyByteString "" `shouldThrow` \x ->
        x == ParsingError 0 "not enough bytes"
    it "decodes inline table fields" $ do
      let bs = root $ encodeMyRoot (int32 minBound) (int64 maxBound) (encodeNested (int32 123))
      s <- myRootFromLazyByteString bs

      myRootA s `shouldBe` Just minBound
      myRootB s `shouldBe` Just maxBound
      
    it "decodes missing fields" $ do
      let bs = root $ encodeMyRoot missing missing (encodeNested missing)
      s <- myRootFromLazyByteString bs
      
      myRootA s `shouldBe` Just 0
      myRootB s `shouldBe` Just 0

      nested <- myRootC s
      nestedA nested `shouldBe` Just 0

    it "decodes nested tables" $ do
      let bs = root $ encodeMyRoot (int32 99) (int64 maxBound) (encodeNested (int32 123))
      s <- myRootFromLazyByteString bs

      nested <- myRootC s
      nestedA nested `shouldBe` Just 123


newtype MyRoot =
  MyRoot Table

myRootFromLazyByteString :: ReadCtx m => ByteString -> m MyRoot
myRootFromLazyByteString bs = MyRoot <$> fromLazyByteString bs

encodeMyRoot :: Tagged Int32 Field -> Tagged Int64 Field -> Tagged Nested Field -> Tagged MyRoot Field
encodeMyRoot a b c =
  Tagged $ F.table
    [ untag a
    , untag b
    , untag c
    ]

myRootA :: ReadCtx m => MyRoot -> m Int32
myRootA (MyRoot t) = readInt32 t 0 0

myRootB :: ReadCtx m => MyRoot -> m Int64
myRootB (MyRoot t) = readInt64 t 1 0

myRootC :: ReadCtx m => MyRoot -> m Nested
myRootC (MyRoot t) = Nested <$> readTableReq t 2 "c"


newtype Nested =
  Nested Table

nestedFromLazyByteString :: ReadCtx m => ByteString -> m Nested
nestedFromLazyByteString bs = Nested <$> fromLazyByteString bs

encodeNested :: Tagged Int32 Field -> Tagged Nested Field
encodeNested a =
  Tagged $ F.table
    [ untag a ]

nestedA :: ReadCtx m => Nested -> m Int32
nestedA (Nested t) = readInt32 t 0 0
    