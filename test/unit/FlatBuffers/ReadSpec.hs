{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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

    let missingFields = root $ encodeMyRoot missing missing missing missing missing
    
    it "throws when string is missing" $ do
      s <- myRootFromLazyByteString missingFields
      myRootD s `shouldThrow` \x -> x == MissingField "d"

    it "throws when table is missing" $ do
      s <- myRootFromLazyByteString missingFields
      myRootC s `shouldThrow` \x -> x == MissingField "c"

    it "throws when struct is missing" $ do
      s <- myRootFromLazyByteString missingFields
      myRootE s `shouldThrow` \x -> x == MissingField "e"
      
    
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
  deriving (HasPosition, HasTable)

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
myRootA x = tableIndexToVOffset x 0 >>= optional 0 (readNumerical . move x)

myRootB :: ReadCtx m => MyRoot -> m Int64
myRootB x = tableIndexToVOffset x 1 >>= optional 0 (readNumerical . move x)

myRootC :: ReadCtx m => MyRoot -> m Nested
myRootC x = tableIndexToVOffset x 2 >>= required "c" (readTable . move x) <&> Nested

myRootD :: ReadCtx m => MyRoot -> m T.Text
myRootD x = tableIndexToVOffset x 3 >>= required "d" (readText . move x)

myRootE :: ReadCtx m => MyRoot -> m SWS
myRootE x = tableIndexToVOffset x 4 >>= required "e" (pure . readStruct . move x) <&> SWS

newtype Nested =
  Nested Table
  deriving (HasPosition, HasTable)

encodeNested :: Tagged Int32 Field -> Tagged DeepNested Field -> Tagged Nested Field
encodeNested a b =
  Tagged $ F.table
    [ untag a, untag b ]

nestedA :: ReadCtx m => Nested -> m Int32
nestedA x = tableIndexToVOffset x 0 >>= optional 0 (readNumerical . move x)

nestedB :: ReadCtx m => Nested -> m DeepNested
nestedB x = tableIndexToVOffset x 1 >>= required "b" (readTable . move x) <&> DeepNested
    
newtype DeepNested = DeepNested Table
  deriving (HasTable, HasPosition)

encodeDeepNested :: Tagged Int32 Field -> Tagged DeepNested Field
encodeDeepNested a =
  Tagged $ F.table
    [ untag a ]

deepNestedA :: ReadCtx m => DeepNested -> m Int32
deepNestedA x = tableIndexToVOffset x 0 >>= optional 0 (readNumerical . move x)

newtype MyStruct =
  MyStruct Struct
  deriving HasPosition

encodeMyStruct :: Int32 -> Word8 -> Int64 -> Tagged MyStruct Field
encodeMyStruct a b c =
  Tagged $ F.scalar F.struct
    [ F.int32 a
    , F.padded 3 $ F.word8 b
    , F.int64 c
    ]

myStructA :: ReadCtx m => MyStruct -> m Int32
myStructA x = readNumerical $ move x 0

myStructB :: ReadCtx m => MyStruct -> m Word8
myStructB x = readNumerical $ move x 4

myStructC :: ReadCtx m => MyStruct -> m Int64
myStructC x = readNumerical $ move x 8

newtype ThreeBytes = ThreeBytes Struct
  deriving HasPosition

encodeThreeBytes :: Word8 -> Word8 -> Word8 -> Tagged ThreeBytes Field
encodeThreeBytes a b c =
  Tagged $ F.scalar F.struct
    [ F.word8 a
    , F.word8 b
    , F.word8 c
    ]

threeBytesA :: ReadCtx m => ThreeBytes -> m Word8
threeBytesA x = readNumerical $ move x 0

threeBytesB :: ReadCtx m => ThreeBytes -> m Word8
threeBytesB x = readNumerical $ move x 1

threeBytesC :: ReadCtx m => ThreeBytes -> m Word8
threeBytesC x = readNumerical $ move x 2

-- struct with structs
newtype SWS = SWS Struct
  deriving HasPosition

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
swsA x = MyStruct . readStruct $ move x 0

swsB :: SWS -> ThreeBytes
swsB x = ThreeBytes . readStruct $ move x 16
