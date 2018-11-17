{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.ReadSpec where

import qualified Data.ByteString.Builder as B
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Int
import           Data.Tagged             (Tagged (..), untag)
import qualified Data.Text               as T
import           Data.Word
import qualified FlatBuffers             as F
import           FlatBuffers.Classes     (dflt)
import           FlatBuffers.Dsl
import           FlatBuffers.Read
import           Test.Hspec

spec :: Spec
spec =
  describe "read" $ do 
    it "throws when buffer is exhausted" $
      fromLazyByteString "" `shouldThrow` \x ->
        x == ParsingError 0 "not enough bytes"

    it "throws when string is missing" $ do
      let bs = root $ encodeMyRoot missing missing missing missing missing
      s <- myRootFromLazyByteString bs
      myRootD s `shouldThrow` \x -> x == MissingField "d"

    it "throws when table is missing" $ do
      let bs = root $ encodeMyRoot missing missing missing missing missing
      s <- myRootFromLazyByteString bs
      myRootC s `shouldThrow` \x -> x == MissingField "c"
      
    it "throws when string is invalid utf-8" $ do
      let text = Tagged $ F.vector [F.scalar F.word8 255]
      let bs = root $ encodeMyRoot missing missing missing text missing
      s <- myRootFromLazyByteString bs
      myRootD s `shouldThrow` \x ->
        x == Utf8DecodingError "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" (Just 255)

    it "decodes inline table fields" $ do
      let bs = root $ encodeMyRoot (int32 minBound) (int64 maxBound) (encodeNested (int32 123)) (text "hello") missing
      s <- myRootFromLazyByteString bs

      myRootA s `shouldBe` Just minBound
      myRootB s `shouldBe` Just maxBound
      myRootD s `shouldBe` Just "hello"
      
    it "decodes missing fields" $ do
      let bs = root $ encodeMyRoot missing missing (encodeNested missing) missing missing
      s <- myRootFromLazyByteString bs
      
      myRootA s `shouldBe` Just 0
      myRootB s `shouldBe` Just 0

      nested <- myRootC s
      nestedA nested `shouldBe` Just 0

    it "decodes nested tables" $ do
      let bs = root $ encodeMyRoot (int32 99) (int64 maxBound) (encodeNested (int32 123)) (text "hello") missing
      s <- myRootFromLazyByteString bs

      nested <- myRootC s
      nestedA nested `shouldBe` Just 123

    it "decodes structs" $ do
      let bs = root $ encodeMyRoot (int32 99) (int64 maxBound) (encodeNested (int32 123)) (text "hello")
            (encodeMyStruct 123 234 456)
      s <- myRootFromLazyByteString bs

      ms <- myRootE s
      myStructA ms `shouldBe` Just 123
      myStructB ms `shouldBe` Just 234
      myStructC ms `shouldBe` Just 456


newtype MyRoot =
  MyRoot Table

myRootFromLazyByteString :: ReadCtx m => ByteString -> m MyRoot
myRootFromLazyByteString bs = MyRoot <$> fromLazyByteString bs

encodeMyRoot ::
     Tagged Int32 Field
  -> Tagged Int64 Field
  -> Tagged Nested Field
  -> Tagged T.Text Field
  -> Tagged MyStruct Field
  -> Tagged MyRoot Field
encodeMyRoot a b c d e = Tagged $ F.table [untag a, untag b, untag c, untag d, untag e]

myRootA :: ReadCtx m => MyRoot -> m Int32
myRootA (MyRoot t) = indexToNumerical t 0 dflt

myRootB :: ReadCtx m => MyRoot -> m Int64
myRootB (MyRoot t) = indexToNumerical t 1 dflt

myRootC :: ReadCtx m => MyRoot -> m Nested
myRootC (MyRoot t) = Nested <$> readTableReq t 2 "c"

myRootD :: ReadCtx m => MyRoot -> m T.Text
myRootD (MyRoot t) = readTextReq t 3 "d"

myRootE :: ReadCtx m => MyRoot -> m MyStruct
myRootE (MyRoot t) = MyStruct <$> readStructReq t 4 "e"

newtype Nested =
  Nested Table

nestedFromLazyByteString :: ReadCtx m => ByteString -> m Nested
nestedFromLazyByteString bs = Nested <$> fromLazyByteString bs

encodeNested :: Tagged Int32 Field -> Tagged Nested Field
encodeNested a =
  Tagged $ F.table
    [ untag a ]

nestedA :: ReadCtx m => Nested -> m Int32
nestedA (Nested t) = indexToNumerical t 0 dflt
    
newtype MyStruct =
  MyStruct Struct

encodeMyStruct :: Int32 -> Word8 -> Int64 -> Tagged MyStruct Field
encodeMyStruct a b c =
  Tagged $ F.scalar F.struct
  [ F.int32 a
  , F.padded 3 $ F.word8 b
  , F.int64 c
  ]

myStructA :: ReadCtx m => MyStruct -> m Int32
myStructA (MyStruct s) = vOffsetToNumerical (struct s) 0

myStructB :: ReadCtx m => MyStruct -> m Word8
myStructB (MyStruct s) = vOffsetToNumerical (struct s) 4

myStructC :: ReadCtx m => MyStruct -> m Int64
myStructC (MyStruct s) = vOffsetToNumerical (struct s) 8

