{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FlatBuffers.Integration.RoundTripThroughFlatcSpec where

import           Control.Exception          ( throwIO )
import           Control.Applicative        ( liftA3 )
import           Data.Aeson                 ( Value(..), object, (.=) )
import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy       as BSL
import           Data.Int
import           Data.Proxy
import           Data.Typeable              ( Typeable, typeRep )
import           Data.Word
import           Data.Maybe (isNothing)
import           Examples.HandWritten
import           FlatBuffers.FileIdentifier ( HasFileIdentifier )
import           FlatBuffers.Read
import           FlatBuffers.Write
import qualified System.Process             as Sys
import qualified System.Directory           as Dir
import           Test.Hspec
import           TestUtils

{-

These tests ensure our encoders/decoders are consistent with flatc's.
Each test:
 - creates a flatbuffer using haskell's encoders
 - saves it to a .bin file
 - asks flatc to convert it to a .json file
 - reads the json into memory, checks it against the expected json
 - asks flatc to convert that .json file back to .bin
 - reads the flatbuffer created by flatc into memory, checks that it contains the same data as we started with.

See "Using flatc as a Conversion Tool" at the bottom:
  https://google.github.io/flatbuffers/flatbuffers_guide_tutorial.html

Note that flatc is not yet able to convert vector of unions from binary to json (even though
json -> binary works), so we can't use flatc to test this.
Instead, we check our encoders against java's decoders by
sending requests to a Scala server: FlatBuffers.Integration.HaskellToScalaSpec.

-}

spec :: Spec
spec =
  describe "Haskell encoders/decoders should be consistent with flatc" $
    beforeAll_ (Dir.createDirectoryIfMissing True "temp") $ do
    describe "Primitives" $ do
      it "present with maxBound" $ do
        (json, decoded) <- flatcWithFileIdentifier $ primitives
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just 1234.56) (Just 2873242.82782) (Just True) (Just "hi 👬 bye")

        json `shouldBeJson` object
          [ "a" .= maxBound @Word8
          , "b" .= maxBound @Word16
          , "c" .= maxBound @Word32
          , "d" .= maxBound @Word64
          , "e" .= maxBound @Int8
          , "f" .= maxBound @Int16
          , "g" .= maxBound @Int32
          , "h" .= maxBound @Int64
          , "i" .= Number 1234.560059
          , "j" .= Number 2873242.827819999773
          , "k" .= True
          , "l" .= String "hi 👬 bye"
          ]

        getPrimitives'a decoded `shouldBe` Right maxBound
        getPrimitives'b decoded `shouldBe` Right maxBound
        getPrimitives'c decoded `shouldBe` Right maxBound
        getPrimitives'd decoded `shouldBe` Right maxBound
        getPrimitives'e decoded `shouldBe` Right maxBound
        getPrimitives'f decoded `shouldBe` Right maxBound
        getPrimitives'g decoded `shouldBe` Right maxBound
        getPrimitives'h decoded `shouldBe` Right maxBound
        getPrimitives'i decoded `shouldBe` Right 1234.56
        getPrimitives'j decoded `shouldBe` Right 2873242.82782
        getPrimitives'k decoded `shouldBe` Right True
        getPrimitives'l decoded `shouldBe` Right (Just "hi 👬 bye")

      it "present with minBound" $ do
        (json, decoded) <- flatcWithFileIdentifier $ primitives
          (Just minBound) (Just minBound) (Just minBound) (Just minBound)
          (Just minBound) (Just minBound) (Just minBound) (Just minBound)
          (Just 1234.56) (Just 2873242.82782) (Just False) (Just "hi 👬 bye")

        json `shouldBeJson` object
          [ "e" .= minBound @Int8
          , "f" .= minBound @Int16
          , "g" .= minBound @Int32
          , "h" .= minBound @Int64
          , "i" .= Number 1234.560059
          , "j" .= Number 2873242.827819999773
          , "l" .= String "hi 👬 bye"
          ]

        getPrimitives'a decoded `shouldBe` Right minBound
        getPrimitives'b decoded `shouldBe` Right minBound
        getPrimitives'c decoded `shouldBe` Right minBound
        getPrimitives'd decoded `shouldBe` Right minBound
        getPrimitives'e decoded `shouldBe` Right minBound
        getPrimitives'f decoded `shouldBe` Right minBound
        getPrimitives'g decoded `shouldBe` Right minBound
        getPrimitives'h decoded `shouldBe` Right minBound
        getPrimitives'i decoded `shouldBe` Right 1234.56
        getPrimitives'j decoded `shouldBe` Right 2873242.82782
        getPrimitives'k decoded `shouldBe` Right False
        getPrimitives'l decoded `shouldBe` Right (Just "hi 👬 bye")

      it "present with defaults" $ do
        (json, decoded) <- flatcWithFileIdentifier $ primitives
          (Just 0) (Just 0) (Just 0) (Just 0)
          (Just 0) (Just 0) (Just 0) (Just 0)
          (Just 0) (Just 0) (Just False) (Just "hi 👬 bye")

        json `shouldBeJson` object
          [ "l" .= String "hi 👬 bye"
          ]

        getPrimitives'a decoded `shouldBe` Right 0
        getPrimitives'b decoded `shouldBe` Right 0
        getPrimitives'c decoded `shouldBe` Right 0
        getPrimitives'd decoded `shouldBe` Right 0
        getPrimitives'e decoded `shouldBe` Right 0
        getPrimitives'f decoded `shouldBe` Right 0
        getPrimitives'g decoded `shouldBe` Right 0
        getPrimitives'h decoded `shouldBe` Right 0
        getPrimitives'i decoded `shouldBe` Right 0
        getPrimitives'j decoded `shouldBe` Right 0
        getPrimitives'k decoded `shouldBe` Right False
        getPrimitives'l decoded `shouldBe` Right (Just "hi 👬 bye")

      it "missing" $ do
        (json, decoded) <- flatcWithFileIdentifier $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing

        json `shouldBeJson` object []

        getPrimitives'a decoded `shouldBe` Right 0
        getPrimitives'b decoded `shouldBe` Right 0
        getPrimitives'c decoded `shouldBe` Right 0
        getPrimitives'd decoded `shouldBe` Right 0
        getPrimitives'e decoded `shouldBe` Right 0
        getPrimitives'f decoded `shouldBe` Right 0
        getPrimitives'g decoded `shouldBe` Right 0
        getPrimitives'h decoded `shouldBe` Right 0
        getPrimitives'i decoded `shouldBe` Right 0
        getPrimitives'j decoded `shouldBe` Right 0
        getPrimitives'k decoded `shouldBe` Right False
        getPrimitives'l decoded `shouldBe` Right Nothing

    describe "Enums" $ do
      let readStructWithEnum = (liftA3 . liftA3) (,,) getStructWithEnum'x (fmap toColor <$> getStructWithEnum'y) getStructWithEnum'z
      it "present" $ do
        (json, decoded) <- flatc $ enums
          (Just (fromColor ColorGray))
          (Just (structWithEnum 11 (fromColor ColorRed) 22))
          (Just [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen])
          (Just [structWithEnum 33 (fromColor ColorRed) 44, structWithEnum 55 (fromColor ColorGreen) 66])

        json `shouldBeJson` object
          [ "x" .= String "Gray"
          , "y" .= object [ "x" .= Number 11, "y" .= String "Red", "z" .= Number 22 ]
          , "xs" .= [ String "Black", String "Blue", String "Green" ]
          , "ys" .=
            [ object [ "x" .= Number 33, "y" .= String "Red", "z" .= Number 44 ]
            , object [ "x" .= Number 55, "y" .= String "Green", "z" .= Number 66 ]
            ]
          ]

        toColor <$> getEnums'x decoded `shouldBe` Right (Just ColorGray)
        (getEnums'y decoded >>= traverse readStructWithEnum) `shouldBe` Right (Just (11, Just ColorRed, 22))
        (getEnums'xs decoded >>= traverse toList) `shouldBe` Right (Just [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen])
        (getEnums'ys decoded >>= traverse toList >>= traverse (traverse readStructWithEnum)) `shouldBe`
          Right (Just
            [ (33, Just ColorRed, 44)
            , (55, Just ColorGreen, 66)
            ])

      it "present with defaults" $ do
        (json, decoded) <- flatc $ enums
          (Just (fromColor ColorGreen))
          Nothing
          Nothing
          Nothing

        json `shouldBeJson` object [ ]

        toColor <$> getEnums'x decoded `shouldBe` Right (Just ColorGreen)
        getEnums'y decoded `shouldBeRightAnd` isNothing
        getEnums'xs decoded `shouldBeRightAnd` isNothing
        getEnums'ys decoded `shouldBeRightAnd` isNothing

      it "missing" $ do
        (json, decoded) <- flatc $ enums Nothing Nothing Nothing Nothing

        json `shouldBeJson` object [ ]

        toColor <$> getEnums'x decoded `shouldBe` Right (Just ColorGreen)
        getEnums'y decoded `shouldBeRightAnd` isNothing
        getEnums'xs decoded `shouldBeRightAnd` isNothing
        getEnums'ys decoded `shouldBeRightAnd` isNothing

    describe "Structs" $ do
      it "present" $ do
        let readStruct1 = (liftA3 . liftA3) (,,) getStruct1'x getStruct1'y getStruct1'z
        let readStruct2 = getStruct2'x
        let readStruct3 = (liftA3 . liftA3) (,,) (getStruct2'x . getStruct3'x) getStruct3'y getStruct3'z
        let readStruct4 = (liftA4 . liftA4) (,,,) (getStruct2'x . getStruct4'w) getStruct4'x getStruct4'y getStruct4'z
        (json, decoded) <- flatc $ structs
          (Just (struct1 1 2 3))
          (Just (struct2 11))
          (Just (struct3 (struct2 22) 33 44))
          (Just (struct4 (struct2 55) 66 77 True))

        json `shouldBeJson` object
          [ "a" .= object ["x" .= Number 1, "y" .= Number 2, "z" .= Number 3]
          , "b" .= object ["x" .= Number 11]
          , "c" .= object ["x" .= object ["x" .= Number 22], "y" .= Number 33, "z" .= Number 44 ]
          , "d" .= object ["w" .= object ["x" .= Number 55], "x" .= Number 66, "y" .= Number 77, "z" .= True ]
          ]

        s1 <- fromRightJust $ getStructs'a decoded
        s2 <- fromRightJust $ getStructs'b decoded
        s3 <- fromRightJust $ getStructs'c decoded
        s4 <- fromRightJust $ getStructs'd decoded

        readStruct1 s1 `shouldBe` Right (1, 2, 3)
        readStruct2 s2 `shouldBe` Right 11
        readStruct3 s3 `shouldBe` Right (22, 33, 44)
        readStruct4 s4 `shouldBe` Right (55, 66, 77, True)

      it "missing" $ do
        (json, decoded) <- flatc $ structs
          Nothing
          Nothing
          Nothing
          Nothing

        json `shouldBeJson` object [ ]

        getStructs'a decoded `shouldBeRightAnd` isNothing
        getStructs'b decoded `shouldBeRightAnd` isNothing
        getStructs'c decoded `shouldBeRightAnd` isNothing
        getStructs'd decoded `shouldBeRightAnd` isNothing

    describe "Nested tables" $ do
      it "present" $ do
        (json, decoded) <- flatc $ nestedTables (Just (table1 (Just (table2 (Just 11))) (Just 22)))

        json `shouldBeJson` object
          [ "x" .= object
            [ "x" .= object
              [ "x" .= Number 11
              ]
            , "y" .= Number 22
            ]
          ]

        t1 <- fromRightJust $ getNestedTables'x decoded
        t2 <- fromRightJust $ getTable1'x t1

        getTable1'y t1 `shouldBe` Right 22
        getTable2'x t2 `shouldBe` Right 11

      it "missing table2" $ do
        (json, decoded) <- flatc $ nestedTables (Just (table1 Nothing (Just 22)))

        json `shouldBeJson` object
          [ "x" .= object
            [ "y" .= Number 22
            ]
          ]

        t1 <- fromRightJust $ getNestedTables'x decoded
        getTable1'x t1 `shouldBeRightAnd` isNothing
        getTable1'y t1 `shouldBe` Right 22

      it "missing table1" $ do
        (json, decoded) <- flatc $ nestedTables Nothing

        json `shouldBeJson` object []

        getNestedTables'x decoded `shouldBeRightAnd` isNothing


    describe "Union" $
      describe "present" $ do
        it "with sword" $ do
          (json, decoded) <- flatc $ tableWithUnion (weapon (sword (Just "hi")))

          json `shouldBeJson` object
            [ "uni"      .= object [ "x" .= String "hi" ]
            , "uni_type" .= String "Sword"
            ]

          getTableWithUnion'uni decoded `shouldBeRightAndExpect` \case
            Union (Weapon'Sword x) -> getSword'x x `shouldBe` Right (Just "hi")
            _                      -> unexpectedUnionType

        it "with axe" $ do
          (json, decoded) <- flatc $ tableWithUnion (weapon (axe (Just maxBound)))

          json `shouldBeJson` object
            [ "uni"      .= object [ "y" .= maxBound @Int32 ]
            , "uni_type" .= String "Axe"
            ]

          getTableWithUnion'uni decoded `shouldBeRightAndExpect` \case
            Union (Weapon'Axe x) -> getAxe'y x `shouldBe` Right maxBound
            _                    -> unexpectedUnionType

        it "with none" $ do
          (json, decoded) <- flatc $ tableWithUnion none

          json `shouldBeJson` object []

          getTableWithUnion'uni decoded `shouldBeRightAndExpect` \case
            UnionNone -> pure ()
            _         -> unexpectedUnionType


    describe "Vectors" $ do
      it "non-empty" $ do
        (json, decoded) <- flatc $ vectors
          (Just [minBound, 0, maxBound])
          (Just [minBound, 0, maxBound])
          (Just [minBound, 0, maxBound])
          (Just [minBound, 0, maxBound])
          (Just [minBound, 0, maxBound])
          (Just [minBound, 0, maxBound])
          (Just [minBound, 0, maxBound])
          (Just [minBound, 0, maxBound])
          (Just [-12e9, 0, 3.333333])
          (Just [-12e98, 0, 3.33333333333333333333])
          (Just [True, False, True])
          (Just ["hi 👬 bye", "", "world"])

        json `shouldBeJson` object
          [ "a" .= [ minBound @Word8, 0, maxBound @Word8 ]
          , "b" .= [ minBound @Word16, 0, maxBound @Word16 ]
          , "c" .= [ minBound @Word32, 0, maxBound @Word32 ]
          , "d" .= [ minBound @Word64, 0, maxBound @Word64 ]
          , "e" .= [ minBound @Int8, 0, maxBound @Int8 ]
          , "f" .= [ minBound @Int16, 0, maxBound @Int16 ]
          , "g" .= [ minBound @Int32, 0, maxBound @Int32 ]
          , "h" .= [ minBound @Int64, 0, maxBound @Int64 ]
          , "i" .= [ Number (-12e9), Number 0, Number 3.333333 ]
          , "j" .= [ Number (-1.200000000000000057936847176226483074592535164143811899621896087972531077696693922075702102406987776e99), Number 0.0, Number 3.333333333333 ]
          , "k" .= [ True, False, True ]
          , "l" .= [ String "hi 👬 bye", String "", String "world"]
          ]

        (getVectors'a decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'b decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'c decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'd decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'e decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'f decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'g decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'h decoded >>= traverse toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (getVectors'i decoded >>= traverse toList) `shouldBe` Right (Just [-12e9, 0, 3.333333])
        (getVectors'j decoded >>= traverse toList) `shouldBe` Right (Just [-12e98, 0, 3.333333333333])
        (getVectors'k decoded >>= traverse toList) `shouldBe` Right (Just [True, False, True])
        (getVectors'l decoded >>= traverse toList) `shouldBe` Right (Just ["hi 👬 bye", "", "world"])

      it "empty" $ do
        (json, decoded) <- flatc $ vectors
          (Just []) (Just []) (Just []) (Just [])
          (Just []) (Just []) (Just []) (Just [])
          (Just []) (Just []) (Just []) (Just [])

        json `shouldBeJson` object
          [ "a" .= [] @Value
          , "b" .= [] @Value
          , "c" .= [] @Value
          , "d" .= [] @Value
          , "e" .= [] @Value
          , "f" .= [] @Value
          , "g" .= [] @Value
          , "h" .= [] @Value
          , "i" .= [] @Value
          , "j" .= [] @Value
          , "k" .= [] @Value
          , "l" .= [] @Value
          ]

        (getVectors'a decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'b decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'c decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'd decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'e decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'f decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'g decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'h decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'i decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'j decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'k decoded >>= traverse toList) `shouldBe` Right (Just [])
        (getVectors'l decoded >>= traverse toList) `shouldBe` Right (Just [])

      it "missing" $ do
        (json, decoded) <- flatc $ vectors
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing

        json `shouldBeJson` object []

        getVectors'a decoded `shouldBeRightAnd` isNothing
        getVectors'b decoded `shouldBeRightAnd` isNothing
        getVectors'c decoded `shouldBeRightAnd` isNothing
        getVectors'd decoded `shouldBeRightAnd` isNothing
        getVectors'e decoded `shouldBeRightAnd` isNothing
        getVectors'f decoded `shouldBeRightAnd` isNothing
        getVectors'g decoded `shouldBeRightAnd` isNothing
        getVectors'h decoded `shouldBeRightAnd` isNothing
        getVectors'i decoded `shouldBeRightAnd` isNothing
        getVectors'j decoded `shouldBeRightAnd` isNothing
        getVectors'k decoded `shouldBeRightAnd` isNothing
        getVectors'l decoded `shouldBeRightAnd` isNothing

    describe "VectorOfTables" $ do
      it "non empty" $ do
        (json, decoded) <- flatc $ vectorOfTables (Just
          [ axe (Just minBound)
          , axe (Just 0)
          , axe (Just maxBound)
          ])

        json `shouldBeJson` object
          [ "xs" .=
            [ object [ "y" .= minBound @Int32 ]
            , object [ ]
            , object [ "y" .= maxBound @Int32 ]
            ]
          ]

        xs <- fromRightJust $ getVectorOfTables'xs decoded
        (toList xs >>= traverse getAxe'y) `shouldBe` Right [minBound, 0, maxBound]

      it "empty" $ do
        (json, decoded) <- flatc $ vectorOfTables (Just [])

        json `shouldBeJson` object [ "xs" .= [] @Value]

        xs <- fromRightJust $ getVectorOfTables'xs decoded
        vectorLength xs `shouldBe` Right 0

      it "missing" $ do
        (json, decoded) <- flatc $ vectorOfTables Nothing

        json `shouldBeJson` object []

        getVectorOfTables'xs decoded `shouldBeRightAnd` isNothing

    describe "VectorOfStructs" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) getStruct1'x getStruct1'y getStruct1'z
      let readStruct2 = getStruct2'x
      let readStruct3 = (liftA3 . liftA3) (,,) (getStruct2'x . getStruct3'x) getStruct3'y getStruct3'z
      let readStruct4 = (liftA4 . liftA4) (,,,) (getStruct2'x . getStruct4'w) getStruct4'x getStruct4'y getStruct4'z

      it "non empty" $ do
        (json, decoded) <- flatc $ vectorOfStructs
          (Just [struct1 1 2 3, struct1 4 5 6])
          (Just [struct2 101, struct2 102, struct2 103])
          (Just [struct3 (struct2 104) 105 106, struct3 (struct2 107) 108 109, struct3 (struct2 110) 111 112])
          (Just [struct4 (struct2 120) 121 122 True, struct4 (struct2 123) 124 125 False, struct4 (struct2 126) 127 128 True])

        json `shouldBeJson` object
          [ "as" .=
            [ object [ "x" .= Number 1, "y" .= Number 2, "z" .= Number 3]
            , object [ "x" .= Number 4, "y" .= Number 5, "z" .= Number 6]
            ]
          , "bs" .=
            [ object ["x" .= Number 101]
            , object ["x" .= Number 102]
            , object ["x" .= Number 103]
            ]
          , "cs" .=
            [ object ["x" .= object ["x" .= Number 104], "y" .= Number 105, "z" .= Number 106 ]
            , object ["x" .= object ["x" .= Number 107], "y" .= Number 108, "z" .= Number 109 ]
            , object ["x" .= object ["x" .= Number 110], "y" .= Number 111, "z" .= Number 112 ]
            ]
          , "ds" .=
            [ object ["w" .= object ["x" .= Number 120], "x" .= Number 121, "y" .= Number 122, "z" .= True ]
            , object ["w" .= object ["x" .= Number 123], "x" .= Number 124, "y" .= Number 125, "z" .= False ]
            , object ["w" .= object ["x" .= Number 126], "x" .= Number 127, "y" .= Number 128, "z" .= True ]
            ]
          ]

        as <- fromRightJust (getVectorOfStructs'as decoded) >>= (fromRight . toList)
        bs <- fromRightJust (getVectorOfStructs'bs decoded) >>= (fromRight . toList)
        cs <- fromRightJust (getVectorOfStructs'cs decoded) >>= (fromRight . toList)
        ds <- fromRightJust (getVectorOfStructs'ds decoded) >>= (fromRight . toList)

        traverse readStruct1 as `shouldBe` Right [(1,2,3), (4,5,6)]
        traverse readStruct2 bs `shouldBe` Right [101, 102, 103]
        traverse readStruct3 cs `shouldBe` Right [(104, 105, 106), (107, 108, 109), (110, 111, 112)]
        traverse readStruct4 ds `shouldBe` Right [(120, 121, 122, True), (123, 124, 125, False), (126, 127, 128, True)]

      it "empty" $ do
        (json, decoded) <- flatc $ vectorOfStructs (Just []) (Just []) (Just []) (Just [])

        json `shouldBeJson` object [ "as" .= [] @Value, "bs" .= [] @Value, "cs" .= [] @Value, "ds" .= [] @Value ]

        as <- fromRightJust $ getVectorOfStructs'as decoded
        bs <- fromRightJust $ getVectorOfStructs'bs decoded
        cs <- fromRightJust $ getVectorOfStructs'cs decoded
        ds <- fromRightJust $ getVectorOfStructs'cs decoded
        vectorLength as `shouldBe` Right 0
        vectorLength bs `shouldBe` Right 0
        vectorLength cs `shouldBe` Right 0
        vectorLength ds `shouldBe` Right 0

      it "missing" $ do
        (json, decoded) <- flatc $ vectorOfStructs Nothing Nothing Nothing Nothing

        json `shouldBeJson` object []

        getVectorOfStructs'as decoded `shouldBeRightAnd` isNothing
        getVectorOfStructs'bs decoded `shouldBeRightAnd` isNothing
        getVectorOfStructs'cs decoded `shouldBeRightAnd` isNothing
        getVectorOfStructs'ds decoded `shouldBeRightAnd` isNothing


    describe "ScalarsWithDefaults" $ do
      let runTest buffer = do
            (json, decoded) <- flatc buffer

            json `shouldBeJson` object [ ]

            getScalarsWithDefaults'a decoded `shouldBe` Right 8
            getScalarsWithDefaults'b decoded `shouldBe` Right 16
            getScalarsWithDefaults'c decoded `shouldBe` Right 32
            getScalarsWithDefaults'd decoded `shouldBe` Right 64
            getScalarsWithDefaults'e decoded `shouldBe` Right (-1)
            getScalarsWithDefaults'f decoded `shouldBe` Right (-2)
            getScalarsWithDefaults'g decoded `shouldBe` Right (-4)
            getScalarsWithDefaults'h decoded `shouldBe` Right (-8)
            getScalarsWithDefaults'i decoded `shouldBe` Right 3.9
            getScalarsWithDefaults'j decoded `shouldBe` Right (-2.3e10)
            getScalarsWithDefaults'k decoded `shouldBe` Right True
            getScalarsWithDefaults'l decoded `shouldBe` Right False
            toColor <$> getScalarsWithDefaults'm decoded `shouldBe` Right (Just ColorBlue)
            toColor <$> getScalarsWithDefaults'n decoded `shouldBe` Right (Just ColorGray)

      it "present with defaults" $ runTest $ scalarsWithDefaults
        (Just 8) (Just 16) (Just 32) (Just 64)
        (Just (-1)) (Just (-2)) (Just (-4)) (Just (-8))
        (Just 3.9) (Just (-2.3e10)) (Just True) (Just False)
        (Just (fromColor ColorBlue)) (Just (fromColor ColorGray))

      it "missing" $ runTest $ scalarsWithDefaults
        Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing
        Nothing Nothing

    it "DeprecatedFields" $ do
      (json, decoded) <- flatc $ deprecatedFields (Just 1) (Just 2) (Just 3) (Just 4)

      json `shouldBeJson` object
        [ "a" .= Number 1
        , "c" .= Number 2
        , "e" .= Number 3
        , "g" .= Number 4
        ]

      getDeprecatedFields'a decoded `shouldBe` Right 1
      getDeprecatedFields'c decoded `shouldBe` Right 2
      getDeprecatedFields'e decoded `shouldBe` Right 3
      getDeprecatedFields'g decoded `shouldBe` Right 4

    it "RequiredFields" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) getStruct1'x getStruct1'y getStruct1'z
      (json, decoded) <- flatc $ requiredFields
        "hello"
        (struct1 11 22 33)
        (axe (Just 44))
        (weapon (sword (Just "a")))
        [55, 66]

      json `shouldBeJson` object
        [ "a" .= String "hello"
        , "b" .= object ["x" .= Number 11, "y" .= Number 22, "z" .= Number 33]
        , "c" .= object ["y" .= Number 44]
        , "d" .= object ["x" .= String "a"]
        , "d_type" .= String "Sword"
        , "e" .= [Number 55, Number 66]
        ]

      getRequiredFields'a decoded `shouldBe` Right "hello"
      (getRequiredFields'b decoded >>= readStruct1) `shouldBe` Right (11, 22, 33)
      (getRequiredFields'c decoded >>= getAxe'y) `shouldBe` Right 44
      getRequiredFields'd decoded `shouldBeRightAndExpect` \case
        Union (Weapon'Sword x) -> getSword'x x `shouldBe` Right (Just "a")
        _                      -> unexpectedUnionType
      (getRequiredFields'e decoded >>= toList) `shouldBe` Right [55, 66]


unexpectedUnionType :: HasCallStack => Expectation
unexpectedUnionType = expectationFailure "Unexpected union type"

flatc :: forall a. Typeable a => WriteTable a -> IO (J.Value, Table a)
flatc table = flatcAux False (encode table)

flatcWithFileIdentifier :: forall a. (HasFileIdentifier a, Typeable a) => WriteTable a -> IO (J.Value, Table a)
flatcWithFileIdentifier table = flatcAux True (encodeWithFileIdentifier table)

flatcAux :: forall a. Typeable a => Bool -> BSL.ByteString -> IO (J.Value, Table a)
flatcAux withFileIdentifier bs = do
  let tableName = show $ typeRep (Proxy @a)

  BSL.writeFile "temp/a.bin" bs

  Sys.callProcess "flatc" $
    (if not withFileIdentifier then ["--raw-binary"] else [])
    <>
    [ "-o", "./temp"
    , "./test/Examples/schema.fbs"
    , "--root-type", "testapi.flatbuffers." <> tableName
    , "--json"
    , "--strict-json"
    , "--"
    , "temp/a.bin"
    ]

  json <- J.eitherDecodeFileStrict' "temp/a.json" >>= \case
    Left err  -> fail $ "Failed to decode flatc's json:\n" <> err
    Right val -> pure val

  Sys.callProcess "cp" ["temp/a.json", "temp/b.json"]

  Sys.callProcess "flatc"
    [ "-o", "./temp"
    , "./test/Examples/schema.fbs"
    , "--root-type", "testapi.flatbuffers." <> tableName
    , "--binary"
    , "--strict-json"
    , "temp/b.json"
    ]

  bs' <- BSL.readFile "temp/b.bin"

  case decode bs' of
    Right table -> pure (json, table)
    Left err    -> throwIO err
