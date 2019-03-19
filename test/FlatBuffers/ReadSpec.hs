{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TypeApplications           #-}

module FlatBuffers.ReadSpec where

import           Data.Int
import           Data.Maybe                 (isNothing)
import           Data.Text                  (Text)
import           Data.Word
import           FlatBuffers.Constants
import qualified FlatBuffers.Internal.Write as F
import           FlatBuffers.Read
import           FlatBuffers.Write
import           Test.Hspec

spec :: Spec
spec =
  describe "read" $ do 
    it "throws when buffer is exhausted" $
      decode @Table "" `shouldThrow` \x ->
        x == ParsingError 0 "not enough bytes"

    let missingFields = encode $ encodeMyRoot Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    
    it "throws when string is missing" $ do
      s <- decode missingFields
      myRootD req s `shouldThrow` \x -> x == MissingField "d"
      myRootD opt s >>= \mb -> isNothing mb `shouldBe` True

    it "throws when table is missing" $ do
      s <- decode missingFields
      myRootC req s `shouldThrow` \x -> x == MissingField "c"
      myRootC opt s >>= \mb -> isNothing mb `shouldBe` True

    it "throws when struct is missing" $ do
      s <- decode missingFields
      myRootE req s `shouldThrow` \x -> x == MissingField "e"
      myRootE opt s >>= \mb -> isNothing mb `shouldBe` True

    it "throws when vector is missing" $ do
      s <- decode missingFields
      myRootF req s `shouldThrow` \x -> x == MissingField "f"
      myRootF opt s >>= \mb -> isNothing mb `shouldBe` True
    
    it "throws when string is invalid utf-8" $ do
      let text = F.vector @[] [F.inline F.word8 255]
      let bs = F.root $ F.table [F.missing, F.missing, F.missing, text]
      s <- decode bs
      myRootD req s `shouldThrow` \x ->
        x == Utf8DecodingError "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" (Just 255)

    it "decodes inline table fields" $ do
      let bs = encode $ encodeMyRoot (Just minBound) (Just maxBound) Nothing (Just "hello") Nothing (Just []) Nothing
      s <- decode bs

      myRootA s `shouldBe` Just minBound
      myRootB s `shouldBe` Just maxBound
      myRootD req s `shouldBe` Just "hello"
      
    it "decodes missing fields" $ do
      let bs = encode $ encodeMyRoot Nothing Nothing (Just (encodeNested Nothing Nothing)) Nothing Nothing (Just []) Nothing
      s <- decode bs
      
      myRootA s `shouldBe` Just 0
      myRootB s `shouldBe` Just 0

      nested <- myRootC req s
      nestedA nested `shouldBe` Just 0

    it "decodes nested tables" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) (Just (encodeNested (Just 123) (Just (encodeDeepNested (Just 234))))) (Just "hello") Nothing (Just []) Nothing
      s <- decode bs

      nested <- myRootC req s
      nestedA nested `shouldBe` Just 123

      deepNested <- nestedB req nested
      deepNestedA deepNested `shouldBe` Just 234

    it "decodes composite structs" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just []) Nothing
      s <- decode bs

      sws <- myRootE req s
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

      vec <- myRootF req s
      vectorLength vec `shouldBe` 2
      vec `index` 0 `shouldBe` Just "hello"
      vec `index` 1 `shouldBe` Just "world"
      vec `index` 2 `shouldThrow` \x -> x == VectorIndexOutOfBounds 2 2
      vec `index` 3 `shouldThrow` \x -> x == VectorIndexOutOfBounds 2 3
      toList vec `shouldBe` Just ["hello", "world"]

    it "decodes vectors of tables" $ do
      let bs = encode $ encodeMyRoot (Just 99) (Just maxBound) Nothing (Just "hello") (Just (encodeSws 1 2 3 4 5 6)) (Just ["hello", "world"])
            (Just [encodeDeepNested (Just 11), encodeDeepNested (Just 22)])
      s <- decode bs

      vec <- myRootG req s
      vectorLength vec `shouldBe` 2
      list <- toList vec
      traverse deepNestedA list `shouldBe` Just [11, 22]


newtype MyRoot =
  MyRoot Table

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
  writeTable [w a, w b, w c, w d, w e, w f, w g]

myRootA :: ReadCtx m => MyRoot -> m Int32
myRootA = readTableFieldWithDef readInt32 0 0

myRootB :: ReadCtx m => MyRoot -> m Int64
myRootB = readTableFieldWithDef readInt64 1 0

myRootC :: ReadCtx m => ReadMode Nested a -> MyRoot -> m a
myRootC = readTableField readTable 2 "c"

myRootD :: ReadCtx m => ReadMode Text a -> MyRoot -> m a
myRootD = readTableField readText 3 "d"

myRootE :: ReadCtx m => ReadMode SWS a -> MyRoot -> m a
myRootE = readTableField (pure . readStruct) 4 "e"

myRootF :: ReadCtx m => ReadMode (Vector Text) a -> MyRoot -> m a
myRootF = readTableField (readVector readText textSize) 5 "f"

myRootG :: ReadCtx m => ReadMode (Vector DeepNested) a -> MyRoot -> m a
myRootG = readTableField (readVector readTable tableSize) 6 "g"

newtype Nested =
  Nested Table

encodeNested :: Maybe Int32 -> Maybe (WriteTable DeepNested) -> WriteTable Nested
encodeNested a b =
  writeTable
    [ w a, w b ]

nestedA :: ReadCtx m => Nested -> m Int32
nestedA = readTableFieldWithDef readInt32 0 0

nestedB :: ReadCtx m => ReadMode DeepNested a -> Nested -> m a
nestedB = readTableField readTable 1 "b"
    
newtype DeepNested = DeepNested Table

encodeDeepNested :: Maybe Int32 -> WriteTable DeepNested
encodeDeepNested a =
  writeTable
    [ w a ]

deepNestedA :: ReadCtx m => DeepNested -> m Int32
deepNestedA = readTableFieldWithDef readInt32 0 0

newtype MyStruct =
  MyStruct Struct

encodeMyStruct :: Int32 -> Word8 -> Int64 -> WriteStruct MyStruct
encodeMyStruct a b c =
  writeStruct Nothing
    [ ws a
    , padded 3 $ ws b
    , ws c
    ]

myStructA :: ReadCtx m => MyStruct -> m Int32
myStructA = readStructField readInt32 0

myStructB :: ReadCtx m => MyStruct -> m Word8
myStructB = readStructField readWord8 4

myStructC :: ReadCtx m => MyStruct -> m Int64
myStructC = readStructField readInt64 8

newtype ThreeBytes = ThreeBytes Struct

encodeThreeBytes :: Word8 -> Word8 -> Word8 -> WriteStruct ThreeBytes
encodeThreeBytes a b c =
  writeStruct Nothing
    [ ws a
    , ws b
    , ws c
    ]

threeBytesA :: ReadCtx m => ThreeBytes -> m Word8
threeBytesA = readStructField readWord8 0

threeBytesB :: ReadCtx m => ThreeBytes -> m Word8
threeBytesB = readStructField readWord8 1

threeBytesC :: ReadCtx m => ThreeBytes -> m Word8
threeBytesC = readStructField readWord8 2

-- struct with structs
newtype SWS = SWS Struct

encodeSws :: Int32 -> Word8 -> Int64 -> Word8 -> Word8 -> Word8 -> WriteStruct SWS
encodeSws myStructA myStructB myStructC threeBytesA threeBytesB threeBytesC =
  writeStruct Nothing
    [ ws myStructA
    , padded 3 $ ws myStructB
    , ws myStructC
    , ws threeBytesA
    , ws threeBytesB
    , padded 5 $ ws threeBytesC
    ]

swsA :: SWS -> MyStruct
swsA = readStructField readStruct 0

swsB :: SWS -> ThreeBytes
swsB = readStructField readStruct 16
