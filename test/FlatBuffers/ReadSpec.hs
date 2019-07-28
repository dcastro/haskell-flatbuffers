{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module FlatBuffers.ReadSpec where

import           Data.Int
import           Data.Text                  ( Text )
import           Data.Word

import           FlatBuffers.Internal.Build
import           FlatBuffers.Internal.Write
import           FlatBuffers.Internal.Read
import           FlatBuffers.Types

import           Test.Hspec

import           TestUtils

spec :: Spec
spec =
  describe "read" $ do
    it "throws when buffer is exhausted" $
      decode @(Table MyRoot) "" `shouldBeLeft` ParsingError 0 "not enough bytes"

    let missingFields = encode $ encodeMyRoot Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    it "throws when string is missing" $ do
      s <- fromRight $ decode missingFields
      myRootD s `shouldBeLeft` MissingField "d"

    it "throws when table is missing" $ do
      s <- fromRight $ decode missingFields
      myRootC s `shouldBeLeft` MissingField "c"

    it "throws when struct is missing" $ do
      s <- fromRight $ decode missingFields
      myRootE s `shouldBeLeft` MissingField "e"

    it "throws when vector is missing" $ do
      s <- fromRight $ decode missingFields
      myRootF s `shouldBeLeft` MissingField "f"

    it "throws when string is invalid utf-8" $ do
      let text = vector' @Word8 [ 255 ]
      let bs = encode $ writeTable [missing, missing, missing, writeVectorTableField text]
      s <- fromRight $ decode bs
      myRootD s `shouldBeLeft`
        Utf8DecodingError "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" (Just 255)

    it "decodes inline table fields" $ do
      let bs = encode $ encodeMyRoot (Just minBound) (Just maxBound) Nothing (Just "hello") Nothing (Just (vector' nil)) Nothing
      s <- fromRight $ decode bs

      myRootA s `shouldBe` Right minBound
      myRootB s `shouldBe` Right maxBound
      myRootD s `shouldBe` Right "hello"

    it "decodes missing fields" $ do
      let bs = encode $ encodeMyRoot Nothing Nothing (Just (encodeNested Nothing Nothing)) Nothing Nothing (Just (vector' nil)) Nothing
      s <- fromRight $ decode bs

      myRootA s `shouldBe` Right 0
      myRootB s `shouldBe` Right 0

      nested <- fromRight $ myRootC s
      nestedA nested `shouldBe` Right 0

    it "decodes nested tables" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) (Just (encodeNested (Just 123) (Just (encodeDeepNested (Just 234))))) (Just "hello") Nothing (Just (vector' nil)) Nothing
      s <- fromRight $ decode bs

      nested <- fromRight $ myRootC s
      nestedA nested `shouldBe` Right 123

      deepNested <- fromRight $ nestedB nested
      deepNestedA deepNested `shouldBe` Right 234

    it "decodes composite structs" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just (vector' nil)) Nothing
      s <- fromRight $ decode bs

      sws <- fromRight $ myRootE s
      let ms = swsA sws
      myStructA ms `shouldBe` Right 1
      myStructB ms `shouldBe` Right 2
      myStructC ms `shouldBe` Right 3

      let tb = swsB sws
      threeBytesA tb `shouldBe` Right 4
      threeBytesB tb `shouldBe` Right 5
      threeBytesC tb `shouldBe` Right 6

    it "decodes vector of strings" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just (vector' ["hello", "world"])) Nothing
      s <- fromRight $ decode bs

      vec <- fromRight $ myRootF s
      vectorLength vec `shouldBe` Right 2
      vec `index` 0 `shouldBe` Right "hello"
      vec `index` 1 `shouldBe` Right "world"
      toList vec `shouldBe` Right ["hello", "world"]

    it "decodes vectors of tables" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just (vector' ["hello", "world"]))
            (Just (vector' [encodeDeepNested (Just 11), encodeDeepNested (Just 22)]))
      s <- fromRight $ decode bs

      vec <- fromRight $ myRootG s
      vectorLength vec `shouldBe` Right 2
      list <- fromRight $ toList vec
      traverse deepNestedA list `shouldBe` Right [11, 22]

-- | Helper to disambiguate `[]` in the presence of `OverloadedLists`
nil :: [a]
nil = []

data MyRoot

encodeMyRoot ::
     Maybe Int32
  -> Maybe Int64
  -> Maybe (WriteTable Nested)
  -> Maybe Text
  -> Maybe (WriteStruct SWS)
  -> Maybe (WriteVector Text)
  -> Maybe (WriteVector (WriteTable DeepNested))
  -> WriteTable MyRoot
encodeMyRoot a b c d e f g =
  writeTable
    [ optionalDef 0 writeInt32TableField a
    , optionalDef 0 writeInt64TableField b
    , optional writeTableTableField c
    , optional writeTextTableField d
    , optional writeStructTableField e
    , optional writeVectorTableField f
    , optional writeVectorTableField g
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
myRootF = readTableFieldReq (readPrimVector VectorText) 5 "f"

myRootG :: ReadCtx m => Table MyRoot -> m (Vector (Table DeepNested))
myRootG = readTableFieldReq readTableVector 6 "g"

data Nested

encodeNested :: Maybe Int32 -> Maybe (WriteTable DeepNested) -> WriteTable Nested
encodeNested a b =
  writeTable
    [ optionalDef 0 writeInt32TableField a
    , optional writeTableTableField b
    ]

nestedA :: ReadCtx m => Table Nested -> m Int32
nestedA = readTableFieldWithDef readInt32 0 0

nestedB :: ReadCtx m => Table Nested -> m (Table DeepNested)
nestedB = readTableFieldReq readTable 1 "b"

data DeepNested

encodeDeepNested :: Maybe Int32 -> WriteTable DeepNested
encodeDeepNested a =
  writeTable
    [ optionalDef 0 writeInt32TableField a
    ]

deepNestedA :: ReadCtx m => Table DeepNested -> m Int32
deepNestedA = readTableFieldWithDef readInt32 0 0

data MyStruct
instance IsStruct MyStruct where
  structAlignmentOf = 8
  structSizeOf = 16

encodeMyStruct :: Int32 -> Word8 -> Int64 -> WriteStruct MyStruct
encodeMyStruct a b c =
  WriteStruct $
    buildInt32 a
    <> buildWord8 b <> buildPadding 3
    <> buildInt64 c

myStructA :: ReadCtx m => Struct MyStruct -> m Int32
myStructA = readStructField readInt32 0

myStructB :: ReadCtx m => Struct MyStruct -> m Word8
myStructB = readStructField readWord8 4

myStructC :: ReadCtx m => Struct MyStruct -> m Int64
myStructC = readStructField readInt64 8

data ThreeBytes
instance IsStruct ThreeBytes where
  structAlignmentOf = 1
  structSizeOf = 3

encodeThreeBytes :: Word8 -> Word8 -> Word8 -> WriteStruct ThreeBytes
encodeThreeBytes a b c =
  WriteStruct $
    buildWord8 a <> buildWord8 b <> buildWord8 c

threeBytesA :: ReadCtx m => Struct ThreeBytes -> m Word8
threeBytesA = readStructField readWord8 0

threeBytesB :: ReadCtx m => Struct ThreeBytes -> m Word8
threeBytesB = readStructField readWord8 1

threeBytesC :: ReadCtx m => Struct ThreeBytes -> m Word8
threeBytesC = readStructField readWord8 2

-- struct with structs
data SWS
instance IsStruct SWS where
  structAlignmentOf = 8
  structSizeOf = 24

encodeSws :: Int32 -> Word8 -> Int64 -> Word8 -> Word8 -> Word8 -> WriteStruct SWS
encodeSws myStructA myStructB myStructC threeBytesA threeBytesB threeBytesC =
  WriteStruct $
    buildInt32 myStructA
    <> buildWord8 myStructB <> buildPadding 3
    <> buildInt64 myStructC
    <> buildWord8 threeBytesA
    <> buildWord8 threeBytesB
    <> buildWord8 threeBytesC
    <> buildPadding 5

swsA :: Struct SWS -> Struct MyStruct
swsA = readStructField readStruct 0

swsB :: Struct SWS -> Struct ThreeBytes
swsB = readStructField readStruct 16
