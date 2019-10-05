{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use list comprehension" -}

module FlatBuffers.Integration.RoundTripThroughFlatcSpec where

import           Control.Applicative  ( liftA3 )

import           Data.Aeson           ( (.=), Value(..), object, toJSON )
import qualified Data.Aeson           as J
import           Data.Bits            ( (.|.) )
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import           Data.Maybe           ( isNothing )
import           Data.Proxy
import           Data.Typeable        ( Typeable, typeRep )
import           Data.Word

import           Examples

import           FlatBuffers
import qualified FlatBuffers.Vector   as Vec

import qualified System.Directory     as Dir
import qualified System.Process       as Sys

import           TestImports

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

        primitivesA decoded `shouldBe` Right maxBound
        primitivesB decoded `shouldBe` Right maxBound
        primitivesC decoded `shouldBe` Right maxBound
        primitivesD decoded `shouldBe` Right maxBound
        primitivesE decoded `shouldBe` Right maxBound
        primitivesF decoded `shouldBe` Right maxBound
        primitivesG decoded `shouldBe` Right maxBound
        primitivesH decoded `shouldBe` Right maxBound
        primitivesI decoded `shouldBe` Right 1234.56
        primitivesJ decoded `shouldBe` Right 2873242.82782
        primitivesK decoded `shouldBe` Right True
        primitivesL decoded `shouldBe` Right (Just "hi 👬 bye")

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

        primitivesA decoded `shouldBe` Right minBound
        primitivesB decoded `shouldBe` Right minBound
        primitivesC decoded `shouldBe` Right minBound
        primitivesD decoded `shouldBe` Right minBound
        primitivesE decoded `shouldBe` Right minBound
        primitivesF decoded `shouldBe` Right minBound
        primitivesG decoded `shouldBe` Right minBound
        primitivesH decoded `shouldBe` Right minBound
        primitivesI decoded `shouldBe` Right 1234.56
        primitivesJ decoded `shouldBe` Right 2873242.82782
        primitivesK decoded `shouldBe` Right False
        primitivesL decoded `shouldBe` Right (Just "hi 👬 bye")

      it "present with defaults" $ do
        (json, decoded) <- flatcWithFileIdentifier $ primitives
          (Just 0) (Just 0) (Just 0) (Just 0)
          (Just 0) (Just 0) (Just 0) (Just 0)
          (Just 0) (Just 0) (Just False) (Just "hi 👬 bye")

        json `shouldBeJson` object
          [ "l" .= String "hi 👬 bye"
          ]

        primitivesA decoded `shouldBe` Right 0
        primitivesB decoded `shouldBe` Right 0
        primitivesC decoded `shouldBe` Right 0
        primitivesD decoded `shouldBe` Right 0
        primitivesE decoded `shouldBe` Right 0
        primitivesF decoded `shouldBe` Right 0
        primitivesG decoded `shouldBe` Right 0
        primitivesH decoded `shouldBe` Right 0
        primitivesI decoded `shouldBe` Right 0
        primitivesJ decoded `shouldBe` Right 0
        primitivesK decoded `shouldBe` Right False
        primitivesL decoded `shouldBe` Right (Just "hi 👬 bye")

      it "missing" $ do
        (json, decoded) <- flatcWithFileIdentifier $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing

        json `shouldBeJson` object []

        primitivesA decoded `shouldBe` Right 0
        primitivesB decoded `shouldBe` Right 0
        primitivesC decoded `shouldBe` Right 0
        primitivesD decoded `shouldBe` Right 0
        primitivesE decoded `shouldBe` Right 0
        primitivesF decoded `shouldBe` Right 0
        primitivesG decoded `shouldBe` Right 0
        primitivesH decoded `shouldBe` Right 0
        primitivesI decoded `shouldBe` Right 0
        primitivesJ decoded `shouldBe` Right 0
        primitivesK decoded `shouldBe` Right False
        primitivesL decoded `shouldBe` Right Nothing

    describe "Enums" $ do
      let readStructWithEnum = (liftA3 . liftA3) (,,) structWithEnumX (fmap toColor <$> structWithEnumY) structWithEnumZ
      it "present" $ do
        (json, decoded) <- flatc $ enums
          (Just (fromColor ColorGray))
          (Just (structWithEnum 11 (fromColor ColorRed) 22))
          (Just (Vec.fromList' [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen]))
          (Just (Vec.fromList' [structWithEnum 33 (fromColor ColorRed) 44, structWithEnum 55 (fromColor ColorGreen) 66]))

        json `shouldBeJson` object
          [ "x" .= String "Gray"
          , "y" .= object [ "x" .= Number 11, "y" .= String "Red", "z" .= Number 22 ]
          , "xs" .= [ String "Black", String "Blue", String "Green" ]
          , "ys" .=
            [ object [ "x" .= Number 33, "y" .= String "Red", "z" .= Number 44 ]
            , object [ "x" .= Number 55, "y" .= String "Green", "z" .= Number 66 ]
            ]
          ]

        toColor <$> enumsX decoded `shouldBe` Right (Just ColorGray)
        (enumsY decoded >>= traverse readStructWithEnum) `shouldBe` Right (Just (11, Just ColorRed, 22))
        (enumsXs decoded >>= traverse Vec.toList) `shouldBe` Right (Just [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen])
        (enumsYs decoded >>= traverse Vec.toList >>= traverse (traverse readStructWithEnum)) `shouldBe`
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

        toColor <$> enumsX decoded `shouldBe` Right (Just ColorGreen)
        enumsY decoded `shouldBeRightAnd` isNothing
        enumsXs decoded `shouldBeRightAnd` isNothing
        enumsYs decoded `shouldBeRightAnd` isNothing

      it "missing" $ do
        (json, decoded) <- flatc $ enums Nothing Nothing Nothing Nothing

        json `shouldBeJson` object [ ]

        toColor <$> enumsX decoded `shouldBe` Right (Just ColorGreen)
        enumsY decoded `shouldBeRightAnd` isNothing
        enumsXs decoded `shouldBeRightAnd` isNothing
        enumsYs decoded `shouldBeRightAnd` isNothing

    describe "Enums with bit_flags" $ do
      it "present" $ do
        (json, decoded) <- flatc $ enumsBitFlags
          (Just (colorsRed .|. colorsGreen))
          (Just (structWithEnumBitFlags (colorsGreen .|. colorsGray)))
          (Just (Vec.fromList'
            [ colorsGreen .|. colorsGray
            , colorsBlack .|. colorsBlue
            , colorsGreen
            ]))
          (Just (Vec.fromList'
            [ structWithEnumBitFlags (colorsGreen .|. colorsGray)
            , structWithEnumBitFlags (colorsBlack .|. colorsBlue)
            , structWithEnumBitFlags colorsGreen
            ]))

        json `shouldBeJson` object
          [ "x" .= (colorsRed .|. colorsGreen)
          , "y" .= object [ "x" .= (colorsGreen .|. colorsGray) ]
          , "xs" .=
            [ toJSON (colorsGreen .|. colorsGray)
            , toJSON (colorsBlack .|. colorsBlue)
            , String "Green"
            ]
          , "ys" .=
            [ object [ "x" .= (colorsGreen .|. colorsGray) ]
            , object [ "x" .= (colorsBlack .|. colorsBlue) ]
            , object [ "x" .= String "Green" ]
            ]
          ]

        enumsBitFlagsX decoded `shouldBe` Right (colorsRed .|. colorsGreen)
        (enumsBitFlagsY decoded >>= traverse structWithEnumBitFlagsX) `shouldBe` Right (Just (colorsGreen .|. colorsGray))
        (enumsBitFlagsXs decoded >>= traverse Vec.toList) `shouldBe` Right (Just
          [ colorsGreen .|. colorsGray
          , colorsBlack .|. colorsBlue
          , colorsGreen
          ])
        (enumsBitFlagsYs decoded >>= traverse Vec.toList >>= traverse (traverse structWithEnumBitFlagsX)) `shouldBe` Right (Just
          [ colorsGreen .|. colorsGray
          , colorsBlack .|. colorsBlue
          , colorsGreen
          ])

      it "present with defaults" $ do
        (json, decoded) <- flatc $ enumsBitFlags
          (Just 0)
          Nothing
          Nothing
          Nothing

        json `shouldBeJson` object [ ]

        enumsBitFlagsX decoded `shouldBe` Right 0
        enumsBitFlagsY decoded `shouldBeRightAnd` isNothing
        enumsBitFlagsXs decoded `shouldBeRightAnd` isNothing
        enumsBitFlagsYs decoded `shouldBeRightAnd` isNothing

      it "missing" $ do
        (json, decoded) <- flatc $ enumsBitFlags Nothing Nothing Nothing Nothing

        json `shouldBeJson` object [ ]

        enumsBitFlagsX decoded `shouldBe` Right 0
        enumsBitFlagsY decoded `shouldBeRightAnd` isNothing
        enumsBitFlagsXs decoded `shouldBeRightAnd` isNothing
        enumsBitFlagsYs decoded `shouldBeRightAnd` isNothing

    describe "Structs" $ do
      it "present" $ do
        let readStruct1 = (liftA3 . liftA3) (,,) struct1X struct1Y struct1Z
        let readStruct2 = struct2X
        let readStruct3 = (liftA3 . liftA3) (,,) (struct2X . struct3X) struct3Y struct3Z
        let readStruct4 = (liftA4 . liftA4) (,,,) (struct2X . struct4W) struct4X struct4Y struct4Z
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

        s1 <- evalRightJust $ structsA decoded
        s2 <- evalRightJust $ structsB decoded
        s3 <- evalRightJust $ structsC decoded
        s4 <- evalRightJust $ structsD decoded

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

        structsA decoded `shouldBeRightAnd` isNothing
        structsB decoded `shouldBeRightAnd` isNothing
        structsC decoded `shouldBeRightAnd` isNothing
        structsD decoded `shouldBeRightAnd` isNothing

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

        t1 <- evalRightJust $ nestedTablesX decoded
        t2 <- evalRightJust $ table1X t1

        table1Y t1 `shouldBe` Right 22
        table2X t2 `shouldBe` Right 11

      it "missing table2" $ do
        (json, decoded) <- flatc $ nestedTables (Just (table1 Nothing (Just 22)))

        json `shouldBeJson` object
          [ "x" .= object
            [ "y" .= Number 22
            ]
          ]

        t1 <- evalRightJust $ nestedTablesX decoded
        table1X t1 `shouldBeRightAnd` isNothing
        table1Y t1 `shouldBe` Right 22

      it "missing table1" $ do
        (json, decoded) <- flatc $ nestedTables Nothing

        json `shouldBeJson` object []

        nestedTablesX decoded `shouldBeRightAnd` isNothing


    describe "Union" $
      describe "present" $ do
        it "with sword" $ do
          (json, decoded) <- flatc $ tableWithUnion (weaponSword (sword (Just "hi")))

          json `shouldBeJson` object
            [ "uni"      .= object [ "x" .= String "hi" ]
            , "uni_type" .= String "Sword"
            ]

          tableWithUnionUni decoded `shouldBeRightAndExpect` \case
            Union (WeaponSword x) -> swordX x `shouldBe` Right (Just "hi")

        it "with axe" $ do
          (json, decoded) <- flatc $ tableWithUnion (weaponAxe (axe (Just maxBound)))

          json `shouldBeJson` object
            [ "uni"      .= object [ "y" .= maxBound @Int32 ]
            , "uni_type" .= String "Axe"
            ]

          tableWithUnionUni decoded `shouldBeRightAndExpect` \case
            Union (WeaponAxe x) -> axeY x `shouldBe` Right maxBound

        it "with none" $ do
          (json, decoded) <- flatc $ tableWithUnion none

          json `shouldBeJson` object []

          tableWithUnionUni decoded `shouldBeRightAndExpect` \case
            UnionNone -> pure ()


    describe "Vectors" $ do
      it "non-empty" $ do
        (json, decoded) <- flatc $ vectors
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [minBound, 0, maxBound]))
          (Just (Vec.fromList' [-12e9, 0, 3.333333]))
          (Just (Vec.fromList' [-12e98, 0, 3.33333333333333333333]))
          (Just (Vec.fromList' [True, False, True]))
          (Just (Vec.fromList' ["hi 👬 bye", "", "world"]))

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

        (vectorsA decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsB decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsC decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsD decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsE decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsF decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsG decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsH decoded >>= traverse Vec.toList) `shouldBe` Right (Just [minBound, 0, maxBound])
        (vectorsI decoded >>= traverse Vec.toList) `shouldBe` Right (Just [-12e9, 0, 3.333333])
        (vectorsJ decoded >>= traverse Vec.toList) `shouldBe` Right (Just [-12e98, 0, 3.333333333333])
        (vectorsK decoded >>= traverse Vec.toList) `shouldBe` Right (Just [True, False, True])
        (vectorsL decoded >>= traverse Vec.toList) `shouldBe` Right (Just ["hi 👬 bye", "", "world"])

      it "empty" $ do
        (json, decoded) <- flatc $ vectors
          (Just Vec.empty) (Just Vec.empty) (Just Vec.empty) (Just Vec.empty)
          (Just Vec.empty) (Just Vec.empty) (Just Vec.empty) (Just Vec.empty)
          (Just Vec.empty) (Just Vec.empty) (Just Vec.empty) (Just Vec.empty)

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

        (vectorsA decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsB decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsC decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsD decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsE decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsF decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsG decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsH decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsI decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsJ decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsK decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])
        (vectorsL decoded >>= traverse Vec.toList) `shouldBe` Right (Just [])

      it "missing" $ do
        (json, decoded) <- flatc $ vectors
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing

        json `shouldBeJson` object []

        vectorsA decoded `shouldBeRightAnd` isNothing
        vectorsB decoded `shouldBeRightAnd` isNothing
        vectorsC decoded `shouldBeRightAnd` isNothing
        vectorsD decoded `shouldBeRightAnd` isNothing
        vectorsE decoded `shouldBeRightAnd` isNothing
        vectorsF decoded `shouldBeRightAnd` isNothing
        vectorsG decoded `shouldBeRightAnd` isNothing
        vectorsH decoded `shouldBeRightAnd` isNothing
        vectorsI decoded `shouldBeRightAnd` isNothing
        vectorsJ decoded `shouldBeRightAnd` isNothing
        vectorsK decoded `shouldBeRightAnd` isNothing
        vectorsL decoded `shouldBeRightAnd` isNothing

    describe "VectorOfTables" $ do
      it "non empty" $ do
        (json, decoded) <- flatc $ vectorOfTables
          (Just $ Vec.fromList'
            [ axe (Just minBound)
            , axe (Just 0)
            , axe (Just maxBound)
            ]
          )

        json `shouldBeJson` object
          [ "xs" .=
            [ object [ "y" .= minBound @Int32 ]
            , object [ ]
            , object [ "y" .= maxBound @Int32 ]
            ]
          ]

        xs <- evalRightJust $ vectorOfTablesXs decoded
        (Vec.toList xs >>= traverse axeY) `shouldBe` Right [minBound, 0, maxBound]

      it "empty" $ do
        (json, decoded) <- flatc $ vectorOfTables (Just Vec.empty)

        json `shouldBeJson` object [ "xs" .= [] @Value]

        xs <- evalRightJust $ vectorOfTablesXs decoded
        Vec.length xs `shouldBe` 0

      it "missing" $ do
        (json, decoded) <- flatc $ vectorOfTables Nothing

        json `shouldBeJson` object []

        vectorOfTablesXs decoded `shouldBeRightAnd` isNothing

    describe "VectorOfStructs" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) struct1X struct1Y struct1Z
      let readStruct2 = struct2X
      let readStruct3 = (liftA3 . liftA3) (,,) (struct2X . struct3X) struct3Y struct3Z
      let readStruct4 = (liftA4 . liftA4) (,,,) (struct2X . struct4W) struct4X struct4Y struct4Z

      it "non empty" $ do
        (json, decoded) <- flatc $ vectorOfStructs
          (Just (Vec.fromList' [struct1 1 2 3, struct1 4 5 6]))
          (Just (Vec.fromList' [struct2 101, struct2 102, struct2 103]))
          (Just (Vec.fromList' [struct3 (struct2 104) 105 106, struct3 (struct2 107) 108 109, struct3 (struct2 110) 111 112]))
          (Just (Vec.fromList' [struct4 (struct2 120) 121 122 True, struct4 (struct2 123) 124 125 False, struct4 (struct2 126) 127 128 True]))

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

        as <- evalRightJust (vectorOfStructsAs decoded) >>= (evalRight . Vec.toList)
        bs <- evalRightJust (vectorOfStructsBs decoded) >>= (evalRight . Vec.toList)
        cs <- evalRightJust (vectorOfStructsCs decoded) >>= (evalRight . Vec.toList)
        ds <- evalRightJust (vectorOfStructsDs decoded) >>= (evalRight . Vec.toList)

        traverse readStruct1 as `shouldBe` Right [(1,2,3), (4,5,6)]
        traverse readStruct2 bs `shouldBe` Right [101, 102, 103]
        traverse readStruct3 cs `shouldBe` Right [(104, 105, 106), (107, 108, 109), (110, 111, 112)]
        traverse readStruct4 ds `shouldBe` Right [(120, 121, 122, True), (123, 124, 125, False), (126, 127, 128, True)]

      it "empty" $ do
        (json, decoded) <- flatc $ vectorOfStructs
          (Just Vec.empty) (Just Vec.empty) (Just Vec.empty) (Just Vec.empty)

        json `shouldBeJson` object [ "as" .= [] @Value, "bs" .= [] @Value, "cs" .= [] @Value, "ds" .= [] @Value ]

        as <- evalRightJust $ vectorOfStructsAs decoded
        bs <- evalRightJust $ vectorOfStructsBs decoded
        cs <- evalRightJust $ vectorOfStructsCs decoded
        ds <- evalRightJust $ vectorOfStructsCs decoded
        Vec.length as `shouldBe` 0
        Vec.length bs `shouldBe` 0
        Vec.length cs `shouldBe` 0
        Vec.length ds `shouldBe` 0

      it "missing" $ do
        (json, decoded) <- flatc $ vectorOfStructs Nothing Nothing Nothing Nothing

        json `shouldBeJson` object []

        vectorOfStructsAs decoded `shouldBeRightAnd` isNothing
        vectorOfStructsBs decoded `shouldBeRightAnd` isNothing
        vectorOfStructsCs decoded `shouldBeRightAnd` isNothing
        vectorOfStructsDs decoded `shouldBeRightAnd` isNothing


    describe "ScalarsWithDefaults" $ do
      let runTest buffer = do
            (json, decoded) <- flatc buffer

            json `shouldBeJson` object [ ]

            scalarsWithDefaultsA decoded `shouldBe` Right 8
            scalarsWithDefaultsB decoded `shouldBe` Right 16
            scalarsWithDefaultsC decoded `shouldBe` Right 32
            scalarsWithDefaultsD decoded `shouldBe` Right 64
            scalarsWithDefaultsE decoded `shouldBe` Right (-1)
            scalarsWithDefaultsF decoded `shouldBe` Right (-2)
            scalarsWithDefaultsG decoded `shouldBe` Right (-4)
            scalarsWithDefaultsH decoded `shouldBe` Right (-8)
            scalarsWithDefaultsI decoded `shouldBe` Right 3.9
            scalarsWithDefaultsJ decoded `shouldBe` Right (-2.3e10)
            scalarsWithDefaultsK decoded `shouldBe` Right True
            scalarsWithDefaultsL decoded `shouldBe` Right False
            toColor <$> scalarsWithDefaultsM decoded `shouldBe` Right (Just ColorBlue)
            toColor <$> scalarsWithDefaultsN decoded `shouldBe` Right (Just ColorGray)

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

      deprecatedFieldsA decoded `shouldBe` Right 1
      deprecatedFieldsC decoded `shouldBe` Right 2
      deprecatedFieldsE decoded `shouldBe` Right 3
      deprecatedFieldsG decoded `shouldBe` Right 4

    it "RequiredFields" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) struct1X struct1Y struct1Z
      (json, decoded) <- flatc $ requiredFields
        "hello"
        (struct1 11 22 33)
        (axe (Just 44))
        (weaponSword (sword (Just "a")))
        (Vec.fromList' [55, 66])

      json `shouldBeJson` object
        [ "a" .= String "hello"
        , "b" .= object ["x" .= Number 11, "y" .= Number 22, "z" .= Number 33]
        , "c" .= object ["y" .= Number 44]
        , "d" .= object ["x" .= String "a"]
        , "d_type" .= String "Sword"
        , "e" .= [Number 55, Number 66]
        ]

      requiredFieldsA decoded `shouldBe` Right "hello"
      (requiredFieldsB decoded >>= readStruct1) `shouldBe` Right (11, 22, 33)
      (requiredFieldsC decoded >>= axeY) `shouldBe` Right 44
      requiredFieldsD decoded `shouldBeRightAndExpect` \case
        Union (WeaponSword x) -> swordX x `shouldBe` Right (Just "a")
      (requiredFieldsE decoded >>= Vec.toList) `shouldBe` Right [55, 66]


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
    , "--root-type", "examples.generated." <> tableName
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
    , "--root-type", "examples.generated." <> tableName
    , "--binary"
    , "--strict-json"
    , "temp/b.json"
    ]

  bs' <- BSL.readFile "temp/b.bin"

  case decode bs' of
    Right table -> pure (json, table)
    Left err    -> fail err
