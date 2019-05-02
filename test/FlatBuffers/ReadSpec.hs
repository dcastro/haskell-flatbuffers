{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FlatBuffers.ReadSpec where

import           Data.Int
import           Data.Maybe                 ( isNothing )
import           Data.Text                  ( Text )
import           Data.Word

import           FlatBuffers.Constants
import qualified FlatBuffers.Internal.Write as W
import           FlatBuffers.Read
import           FlatBuffers.Write

import           Test.Hspec


spec :: Spec
spec =
  describe "read" $ do
    it "throws when buffer is exhausted" $
      decode @(Table MyRoot) "" `shouldThrow` \x ->
        x == ParsingError 0 "not enough bytes"

    let missingFields = encode $ encodeMyRoot Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    it "throws when string is missing" $ do
      s <- decode missingFields
      myRootD s `shouldThrow` \x -> x == MissingField "d"

    it "throws when table is missing" $ do
      s <- decode missingFields
      myRootC s `shouldThrow` \x -> x == MissingField "c"

    it "throws when struct is missing" $ do
      s <- decode missingFields
      myRootE s `shouldThrow` \x -> x == MissingField "e"

    it "throws when vector is missing" $ do
      s <- decode missingFields
      myRootF s `shouldThrow` \x -> x == MissingField "f"

    it "throws when string is invalid utf-8" $ do
      let text = W.vector @[] [inline word8 255]
      let bs = W.root $ W.table [W.missing, W.missing, W.missing, text]
      s <- decode bs
      myRootD s `shouldThrow` \x ->
        x == Utf8DecodingError "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" (Just 255)

    it "decodes inline table fields" $ do
      let bs = encode $ encodeMyRoot (Just minBound) (Just maxBound) Nothing (Just "hello") Nothing (Just []) Nothing
      s <- decode bs

      myRootA s `shouldBe` Just minBound
      myRootB s `shouldBe` Just maxBound
      myRootD s `shouldBe` Just "hello"

    it "decodes missing fields" $ do
      let bs = encode $ encodeMyRoot Nothing Nothing (Just (encodeNested Nothing Nothing)) Nothing Nothing (Just []) Nothing
      s <- decode bs

      myRootA s `shouldBe` Just 0
      myRootB s `shouldBe` Just 0

      nested <- myRootC s
      nestedA nested `shouldBe` Just 0

    it "decodes nested tables" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) (Just (encodeNested (Just 123) (Just (encodeDeepNested (Just 234))))) (Just "hello") Nothing (Just []) Nothing
      s <- decode bs

      nested <- myRootC s
      nestedA nested `shouldBe` Just 123

      deepNested <- nestedB nested
      deepNestedA deepNested `shouldBe` Just 234

    it "decodes composite structs" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just []) Nothing
      s <- decode bs

      sws <- myRootE s
      let ms = swsA sws
      myStructA ms `shouldBe` Just 1
      myStructB ms `shouldBe` Just 2
      myStructC ms `shouldBe` Just 3

      let tb = swsB sws
      threeBytesA tb `shouldBe` Just 4
      threeBytesB tb `shouldBe` Just 5
      threeBytesC tb `shouldBe` Just 6

    it "decodes vector of strings" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just ["hello", "world"]) Nothing
      s <- decode bs

      vec <- myRootF s
      vectorLength vec `shouldBe` Just 2
      vec `index` 0 `shouldBe` Just "hello"
      vec `index` 1 `shouldBe` Just "world"
      toList vec `shouldBe` Just ["hello", "world"]

    it "decodes vectors of tables" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just ["hello", "world"])
            (Just [encodeDeepNested (Just 11), encodeDeepNested (Just 22)])
      s <- decode bs

      vec <- myRootG s
      vectorLength vec `shouldBe` Just 2
      list <- toList vec
      traverse deepNestedA list `shouldBe` Just [11, 22]


data MyRoot

encodeMyRoot ::
     Maybe Int32
  -> Maybe Int64
  -> Maybe (WriteTable Nested)
  -> Maybe Text
  -> Maybe (WriteStruct SWS)
  -> Maybe [Text]
  -> Maybe [WriteTable DeepNested]
  -> WriteTable MyRoot
encodeMyRoot a b c d e f g =
  writeTable
    [ (optionalDef 0 . inline) int32 a
    , (optionalDef 0 . inline) int64 b
    , optional unWriteTable c
    , optional text d
    , optional unWriteStruct e
    , (optional . writeVector) text f
    , (optional . writeVector) unWriteTable g
    ]

myRootA :: ReadCtx m => Table MyRoot -> m Int32
myRootA = readTableFieldWithDef readInt32 0 0

myRootB :: ReadCtx m => Table MyRoot -> m Int64
myRootB = readTableFieldWithDef readInt64 1 0

myRootC :: ReadCtx m => Table MyRoot -> m (Table Nested)
myRootC = readTableFieldReq readTable 2 "c"

myRootD :: ReadCtx m => Table MyRoot -> m Text
myRootD = readTableFieldReq readText 3 "d"

myRootE :: ReadCtx m => Table MyRoot -> m (Struct SWS)
myRootE = readTableFieldReq readStruct' 4 "e"

myRootF :: ReadCtx m => Table MyRoot -> m (Vector Text)
myRootF = readTableFieldReq (readPrimVector TextVec) 5 "f"

myRootG :: ReadCtx m => Table MyRoot -> m (Vector (Table DeepNested))
myRootG = readTableFieldReq readTableVector 6 "g"

data Nested

encodeNested :: Maybe Int32 -> Maybe (WriteTable DeepNested) -> WriteTable Nested
encodeNested a b =
  writeTable
    [ (optionalDef 0 . inline) int32 a
    , optional unWriteTable b
    ]

nestedA :: ReadCtx m => Table Nested -> m Int32
nestedA = readTableFieldWithDef readInt32 0 0

nestedB :: ReadCtx m => Table Nested -> m (Table DeepNested)
nestedB = readTableFieldReq readTable 1 "b"

data DeepNested

encodeDeepNested :: Maybe Int32 -> WriteTable DeepNested
encodeDeepNested a =
  writeTable
    [ (optionalDef 0 . inline) int32 a
    ]

deepNestedA :: ReadCtx m => Table DeepNested -> m Int32
deepNestedA = readTableFieldWithDef readInt32 0 0

data MyStruct

encodeMyStruct :: Int32 -> Word8 -> Int64 -> WriteStruct MyStruct
encodeMyStruct a b c =
  writeStruct 8
    [ int64 c
    , padded 3 $ word8 b
    , int32 a
    ]

myStructA :: ReadCtx m => Struct MyStruct -> m Int32
myStructA = readStructField readInt32 0

myStructB :: ReadCtx m => Struct MyStruct -> m Word8
myStructB = readStructField readWord8 4

myStructC :: ReadCtx m => Struct MyStruct -> m Int64
myStructC = readStructField readInt64 8

data ThreeBytes

encodeThreeBytes :: Word8 -> Word8 -> Word8 -> WriteStruct ThreeBytes
encodeThreeBytes a b c =
  writeStruct 1
    [ word8 c
    , word8 b
    , word8 a
    ]

threeBytesA :: ReadCtx m => Struct ThreeBytes -> m Word8
threeBytesA = readStructField readWord8 0

threeBytesB :: ReadCtx m => Struct ThreeBytes -> m Word8
threeBytesB = readStructField readWord8 1

threeBytesC :: ReadCtx m => Struct ThreeBytes -> m Word8
threeBytesC = readStructField readWord8 2

-- struct with structs
data SWS

encodeSws :: Int32 -> Word8 -> Int64 -> Word8 -> Word8 -> Word8 -> WriteStruct SWS
encodeSws myStructA myStructB myStructC threeBytesA threeBytesB threeBytesC =
  writeStruct 8
    [ padded 5 $ word8 threeBytesC
    , word8 threeBytesB
    , word8 threeBytesA
    , int64 myStructC
    , padded 3 $ word8 myStructB
    , int32 myStructA
    ]

swsA :: Struct SWS -> Struct MyStruct
swsA = readStructField readStruct 0

swsB :: Struct SWS -> Struct ThreeBytes
swsB = readStructField readStruct 16
