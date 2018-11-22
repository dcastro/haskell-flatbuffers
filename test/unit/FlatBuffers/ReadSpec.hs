{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.ReadSpec where

import qualified Data.ByteString.Builder as B
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Functor            ((<&>))
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
      tableFromLazyByteString "" `shouldThrow` \x ->
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
      let bs = root $ encodeMyRoot (int32 minBound) (int64 maxBound) missing (text "hello") missing
      s <- myRootFromLazyByteString bs

      myRootA s `shouldBe` Just minBound
      myRootB s `shouldBe` Just maxBound
      myRootD s `shouldBe` Just "hello"
      
    it "decodes missing fields" $ do
      let bs = root $ encodeMyRoot missing missing (encodeNested missing missing) missing missing
      s <- myRootFromLazyByteString bs
      
      myRootA s `shouldBe` Just 0
      myRootB s `shouldBe` Just 0

      nested <- myRootC s
      nestedA nested `shouldBe` Just 0

    it "decodes nested tables" $ do
      let bs = root $ encodeMyRoot (int32 99) (int64 maxBound) (encodeNested (int32 123) (encodeDeepNested (int32 234))) (text "hello") missing
      s <- myRootFromLazyByteString bs

      nested <- myRootC s
      nestedA nested `shouldBe` Just 123

      deepNested <- nestedB nested
      deepNestedA deepNested `shouldBe` Just 234

    it "decodes composite structs" $ do
      let bs = root $ encodeMyRoot (int32 99) (int64 maxBound) missing (text "hello") (encodeSws 1 2 3 4 5 6)
      s <- myRootFromLazyByteString bs

      sws <- myRootE s
      let ms = swsA sws
      myStructA ms `shouldBe` Just 1
      myStructB ms `shouldBe` Just 2
      myStructC ms `shouldBe` Just 3

      let tb = swsB sws
      threeBytesA tb `shouldBe` Just 4
      threeBytesB tb `shouldBe` Just 5
      threeBytesC tb `shouldBe` Just 6

newtype MyRoot =
  MyRoot Table

myRootFromLazyByteString :: ReadCtx m => ByteString -> m MyRoot
myRootFromLazyByteString bs = MyRoot <$> tableFromLazyByteString bs

encodeMyRoot ::
     Tagged Int32 Field
  -> Tagged Int64 Field
  -> Tagged Nested Field
  -> Tagged T.Text Field
  -> Tagged SWS Field
  -> Tagged MyRoot Field
encodeMyRoot a b c d e = Tagged $ F.table [untag a, untag b, untag c, untag d, untag e]

myRootA :: ReadCtx m => MyRoot -> m Int32
myRootA (MyRoot t) = tableIndexToVOffset t 0 >>= optional 0 (readNumerical (tablePos t))

myRootB :: ReadCtx m => MyRoot -> m Int64
myRootB (MyRoot t) = tableIndexToVOffset t 1 >>= optional 0 (readNumerical (tablePos t))

myRootC :: ReadCtx m => MyRoot -> m Nested
myRootC (MyRoot t) = tableIndexToVOffset t 2 >>= required "c" (readTable (tablePos t)) <&> Nested

myRootD :: ReadCtx m => MyRoot -> m T.Text
myRootD (MyRoot t) = tableIndexToVOffset t 3 >>= required "d" (readText (tablePos t))

myRootE :: ReadCtx m => MyRoot -> m SWS
myRootE (MyRoot t) = tableIndexToVOffset t 4 >>= required "e" (pure . readStruct (tablePos t)) <&> SWS

newtype Nested =
  Nested Table

encodeNested :: Tagged Int32 Field -> Tagged DeepNested Field -> Tagged Nested Field
encodeNested a b =
  Tagged $ F.table
    [ untag a, untag b ]

nestedA :: ReadCtx m => Nested -> m Int32
nestedA (Nested t) = tableIndexToVOffset t 0 >>= optional 0 (readNumerical (tablePos t))

nestedB :: ReadCtx m => Nested -> m DeepNested
nestedB (Nested t) = tableIndexToVOffset t 1 >>= required "b" (readTable (tablePos t)) <&> DeepNested
    
newtype DeepNested = DeepNested Table

encodeDeepNested :: Tagged Int32 Field -> Tagged DeepNested Field
encodeDeepNested a =
  Tagged $ F.table
    [ untag a ]

deepNestedA :: ReadCtx m => DeepNested -> m Int32
deepNestedA (DeepNested t) = tableIndexToVOffset t 0 >>= optional 0 (readNumerical (tablePos t))

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
myStructA (MyStruct s) = readNumerical' s 0

myStructB :: ReadCtx m => MyStruct -> m Word8
myStructB (MyStruct s) = readNumerical' s 4

myStructC :: ReadCtx m => MyStruct -> m Int64
myStructC (MyStruct s) = readNumerical' s 8

newtype ThreeBytes = ThreeBytes Struct

encodeThreeBytes :: Word8 -> Word8 -> Word8 -> Tagged ThreeBytes Field
encodeThreeBytes a b c =
  Tagged $ F.scalar F.struct
    [ F.word8 a
    , F.word8 b
    , F.word8 c
    ]

threeBytesA :: ReadCtx m => ThreeBytes -> m Word8
threeBytesA (ThreeBytes s) = readNumerical' s 0

threeBytesB :: ReadCtx m => ThreeBytes -> m Word8
threeBytesB (ThreeBytes s) = readNumerical' s 1

threeBytesC :: ReadCtx m => ThreeBytes -> m Word8
threeBytesC (ThreeBytes s) = readNumerical' s 2


-- struct with structs
newtype SWS = SWS Struct

encodeSws :: Int32 -> Word8 -> Int64 -> Word8 -> Word8 -> Word8 -> Tagged SWS Field
encodeSws myStructA myStructB myStructC threeBytesA threeBytesB threeBytesC =
  Tagged $ F.scalar F.struct
    [ F.int32 myStructA
    , F.padded 3 $ F.word8 myStructB
    , F.int64 myStructC
    , F.word8 threeBytesA
    , F.word8 threeBytesB
    , F.padded 5 $ F.word8 threeBytesC
    ]

swsA :: SWS -> MyStruct
swsA (SWS (Struct bs)) = MyStruct $ structFromVOffsetReq bs 0

swsB :: SWS -> ThreeBytes
swsB (SWS (Struct bs)) = ThreeBytes $ structFromVOffsetReq bs 16
