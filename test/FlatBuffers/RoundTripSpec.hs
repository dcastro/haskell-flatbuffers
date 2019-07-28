{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FlatBuffers.RoundTripSpec where

import           Control.Applicative        ( liftA3 )

import           Data.Functor               ( (<&>) )
import qualified Data.List                  as L
import           Data.Maybe                 ( isNothing )
import           Data.Word

import           Examples.Generated

import           FlatBuffers.Internal.Write
import           FlatBuffers.Internal.Read

import           Test.Hspec

import           TestUtils

spec :: Spec
spec =
  describe "Round Trip" $ do
    describe "Primitives" $ do
      it "writes file identifier to buffer" $ do
        let bs1 = encodeWithFileIdentifier $ primitives
              Nothing Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing

        checkFileIdentifier @Primitives bs1 `shouldBe` True

        let bs2 = encode $ primitives
              Nothing Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing
              Nothing Nothing Nothing Nothing

        checkFileIdentifier @Primitives bs2 `shouldBe` False

      it "present" $ do
        x <- fromRight $ decode @Primitives $ encodeWithFileIdentifier $ primitives
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just 1234.56) (Just 2873242.82782) (Just True) (Just "hi 👬 bye")

        primitivesA x `shouldBe` Right maxBound
        primitivesB x `shouldBe` Right maxBound
        primitivesC x `shouldBe` Right maxBound
        primitivesD x `shouldBe` Right maxBound
        primitivesE x `shouldBe` Right maxBound
        primitivesF x `shouldBe` Right maxBound
        primitivesG x `shouldBe` Right maxBound
        primitivesH x `shouldBe` Right maxBound
        primitivesI x `shouldBe` Right 1234.56
        primitivesJ x `shouldBe` Right 2873242.82782
        primitivesK x `shouldBe` Right True
        primitivesL x `shouldBe` Right (Just "hi 👬 bye")


      it "missing" $ do
        x <- fromRight $ decode @Primitives $ encodeWithFileIdentifier $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
        primitivesA x `shouldBe` Right 0
        primitivesB x `shouldBe` Right 0
        primitivesC x `shouldBe` Right 0
        primitivesD x `shouldBe` Right 0
        primitivesE x `shouldBe` Right 0
        primitivesF x `shouldBe` Right 0
        primitivesG x `shouldBe` Right 0
        primitivesH x `shouldBe` Right 0
        primitivesI x `shouldBe` Right 0
        primitivesJ x `shouldBe` Right 0
        primitivesK x `shouldBe` Right False
        primitivesL x `shouldBe` Right Nothing

    describe "Enums" $ do
      let readStructWithEnum = (liftA3 . liftA3) (,,) structWithEnumX (fmap toColor <$> structWithEnumY) structWithEnumZ
      it "present" $ do
        x <- fromRight $ decode $ encode $ enums
          (Just (fromColor ColorGray))
          (Just (structWithEnum 11 (fromColor ColorRed) 22))
          (Just (vector' [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen]))
          (Just (vector' [structWithEnum 33 (fromColor ColorRed) 44, structWithEnum 55 (fromColor ColorGreen) 66]))

        toColor <$> enumsX x `shouldBe` Right (Just ColorGray)
        (enumsY x >>= traverse readStructWithEnum) `shouldBe` Right (Just (11, Just ColorRed, 22))
        (enumsXs x >>= traverse toList) `shouldBe` Right (Just [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen])
        (enumsYs x >>= traverse toList >>= traverse (traverse readStructWithEnum)) `shouldBe` Right (Just [(33, Just ColorRed, 44), (55, Just ColorGreen, 66)])

      it "present with defaults" $ do
        x <- fromRight $ decode @Enums $ encode $ enums (Just (fromColor ColorGreen)) Nothing Nothing Nothing

        toColor <$> enumsX x `shouldBe` Right (Just ColorGreen)
        enumsY x `shouldBeRightAnd` isNothing
        enumsXs x `shouldBeRightAnd` isNothing
        enumsYs x `shouldBeRightAnd` isNothing

      it "missing" $ do
        x <- fromRight $ decode @Enums $ encode $ enums Nothing Nothing Nothing Nothing

        toColor <$> enumsX x `shouldBe` Right (Just ColorGreen)
        enumsY x `shouldBeRightAnd` isNothing
        enumsXs x `shouldBeRightAnd` isNothing
        enumsYs x `shouldBeRightAnd` isNothing

    describe "Structs" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) struct1X struct1Y struct1Z
      let readStruct2 = struct2X
      let readStruct3 = (liftA3 . liftA3) (,,) (struct2X . struct3X) struct3Y struct3Z
      let readStruct4 = (liftA4 . liftA4) (,,,) (struct2X . struct4W) struct4X struct4Y struct4Z
      it "present" $ do
        root <- fromRight $ decode $ encode $ structs
                (Just (struct1 1 2 3))
                (Just (struct2 11))
                (Just (struct3 (struct2 22) 33 44))
                (Just (struct4 (struct2 55) 66 77 True))

        s1 <- fromRightJust $ structsA root
        s2 <- fromRightJust $ structsB root
        s3 <- fromRightJust $ structsC root
        s4 <- fromRightJust $ structsD root

        readStruct1 s1 `shouldBe` Right (1, 2, 3)
        readStruct2 s2 `shouldBe` Right 11
        readStruct3 s3 `shouldBe` Right (22, 33, 44)
        readStruct4 s4 `shouldBe` Right (55, 66, 77, True)

      it "missing" $ do
        root <- fromRight $ decode $ encode $ structs Nothing Nothing Nothing Nothing

        structsA root `shouldBeRightAnd` isNothing
        structsB root `shouldBeRightAnd` isNothing
        structsC root `shouldBeRightAnd` isNothing
        structsD root `shouldBeRightAnd` isNothing

    describe "Nested tables" $ do
      it "present" $ do
        root <- fromRight $ decode $ encode $ nestedTables (Just (table1 (Just (table2 (Just 11))) (Just 22)))

        t1 <- fromRightJust $ nestedTablesX root
        t2 <- fromRightJust $ table1X t1

        table1Y t1 `shouldBe` Right 22
        table2X t2 `shouldBe` Right 11

      it "missing table2" $ do
        root <- fromRight $ decode $ encode $ nestedTables (Just (table1 Nothing (Just 22)))

        t1 <- fromRightJust $ nestedTablesX root
        table1X t1 `shouldBeRightAnd` isNothing
        table1Y t1 `shouldBe` Right 22

      it "missing table1" $ do
        root <- fromRight $ decode $ encode $ nestedTables Nothing

        nestedTablesX root `shouldBeRightAnd` isNothing

    describe "Union" $ do
      it "present" $ do
        x <- fromRight $ decode $ encode $ tableWithUnion (weaponSword (sword (Just "hi")))
        tableWithUnionUni x `shouldBeRightAndExpect` \case
          Union (WeaponSword x) -> swordX x `shouldBe` Right (Just "hi")
          _                     -> unexpectedUnionType

        x <- fromRight $ decode $ encode $ tableWithUnion (weaponAxe (axe (Just maxBound)))
        tableWithUnionUni x `shouldBeRightAndExpect` \case
          Union (WeaponAxe x) -> axeY x `shouldBe` Right maxBound
          _                   -> unexpectedUnionType

        x <- fromRight $ decode $ encode $ tableWithUnion none
        tableWithUnionUni x `shouldBeRightAndExpect` \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "returns none when union type is missing" $ do
        x <- fromRight $ decode $ encode $ writeTable [ missing ]
        tableWithUnionUni x `shouldBeRightAndExpect` \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "returns `unknown` when union type is unknown" $ do
        x <- fromRight $ decode $ encode $ writeTable @TableWithUnion [ writeWord8TableField 99, writeTableTableField $ writeTable []]
        tableWithUnionUni x `shouldBeRightAndExpect` \case
          UnionUnknown n -> n `shouldBe` 99
          _              -> unexpectedUnionType

      it "throws when union type is present, but union value is missing" $ do
        x <- fromRight $ decode $ encode $ writeTable @TableWithUnion [ writeWord8TableField 1]
        tableWithUnionUni x `shouldBeLeft` MalformedBuffer "Union: 'union type' found but 'union value' is missing."

    describe "Vectors" $ do
      let Right nonEmptyVecs = decode $ encode $ vectors
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [minBound, 0, maxBound]))
            (Just (vector' [-12e9, 0, 3.33333333333333333333]))
            (Just (vector' [-12e98, 0, 3.33333333333333333333]))
            (Just (vector' [True, False, True]))
            (Just (vector' ["hi 👬 bye", "", "world"]))

      let Right emptyVecs = decode $ encode $ vectors
            (Just (vector' [])) (Just (vector' [])) (Just (vector' [])) (Just (vector' []))
            (Just (vector' [])) (Just (vector' [])) (Just (vector' [])) (Just (vector' []))
            (Just (vector' [])) (Just (vector' [])) (Just (vector' [])) (Just (vector' []))

      let Right missingVecs = decode $ encode $ vectors
            Nothing Nothing Nothing Nothing
            Nothing Nothing Nothing Nothing
            Nothing Nothing Nothing Nothing

      let
        testPrimVector :: (VectorElement a, Show a, Eq a)
          => (Table Vectors -> Either ReadError (Maybe (Vector a)))
          -> [a]
          -> Spec
        testPrimVector getVec expectedList = do
          it "non empty" $ do
            vec <- fromRightJust (getVec nonEmptyVecs)
            vectorLength vec `shouldBe` Right (L.genericLength expectedList)
            toList vec       `shouldBe` Right expectedList
            traverse (\i -> vec `index` i) [0 .. L.genericLength expectedList - 1] `shouldBe` Right expectedList

          it "empty" $ do
            vec <- fromRightJust (getVec emptyVecs)
            vectorLength vec `shouldBe` Right 0
            toList vec       `shouldBe` Right []

          it "missing" $
            getVec missingVecs `shouldBeRightAnd` isNothing

      describe "word8 vector"  $ testPrimVector vectorsA [minBound, 0, maxBound]
      describe "word16 vector" $ testPrimVector vectorsB [minBound, 0, maxBound]
      describe "word32 vector" $ testPrimVector vectorsC [minBound, 0, maxBound]
      describe "word64 vector" $ testPrimVector vectorsD [minBound, 0, maxBound]
      describe "int8 vector"   $ testPrimVector vectorsE [minBound, 0, maxBound]
      describe "int16 vector"  $ testPrimVector vectorsF [minBound, 0, maxBound]
      describe "int32 vector"  $ testPrimVector vectorsG [minBound, 0, maxBound]
      describe "int64 vector"  $ testPrimVector vectorsH [minBound, 0, maxBound]
      describe "float vector"  $ testPrimVector vectorsI [-12e9, 0, 3.33333333333333333333]
      describe "double vector" $ testPrimVector vectorsJ [-12e98, 0, 3.33333333333333333333]
      describe "bool vector"   $ testPrimVector vectorsK [True, False, True]
      describe "string vector" $ testPrimVector vectorsL ["hi 👬 bye", "", "world"]

    describe "VectorOfTables" $ do
      it "non empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfTables
          (Just $ vector'
            [ axe (Just minBound)
            , axe (Just 0)
            , axe (Just maxBound)
            ]
          )

        Just xs <- fromRight $ vectorOfTablesXs x
        vectorLength xs `shouldBe` Right 3
        (toList xs >>= traverse axeY) `shouldBe` Right [minBound, 0, maxBound]
        (traverse (index xs) [0..2] >>= traverse axeY) `shouldBe` Right [minBound, 0, maxBound]

      it "empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfTables (Just (vector' []))

        xs <- fromRightJust $ vectorOfTablesXs x
        vectorLength xs `shouldBe` Right 0
        (toList xs >>= traverse axeY) `shouldBe` Right []

      it "missing" $ do
        x <- fromRight $ decode $ encode $ vectorOfTables Nothing
        vectorOfTablesXs x `shouldBeRightAnd` isNothing

    describe "VectorOfStructs" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) struct1X struct1Y struct1Z
      let readStruct2 = struct2X
      let readStruct3 = (liftA3 . liftA3) (,,) (struct2X . struct3X) struct3Y struct3Z
      let readStruct4 = (liftA4 . liftA4) (,,,) (struct2X . struct4W) struct4X struct4Y struct4Z

      it "non empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfStructs
          (Just (vector' [struct1 1 2 3, struct1 4 5 6]))
          (Just (vector' [struct2 101, struct2 102, struct2 103]))
          (Just (vector' [struct3 (struct2 104) 105 106, struct3 (struct2 107) 108 109, struct3 (struct2 110) 111 112]))
          (Just (vector' [struct4 (struct2 120) 121 122 True, struct4 (struct2 123) 124 125 False, struct4 (struct2 126) 127 128 True]))

        as <- fromRightJust $ vectorOfStructsAs x
        bs <- fromRightJust $ vectorOfStructsBs x
        cs <- fromRightJust $ vectorOfStructsCs x
        ds <- fromRightJust $ vectorOfStructsDs x

        vectorLength as `shouldBe` Right 2
        (toList as >>= traverse readStruct1) `shouldBe` Right [(1,2,3), (4,5,6)]
        (traverse (index as) [0..1] >>= traverse readStruct1) `shouldBe` Right [(1,2,3), (4,5,6)]

        vectorLength bs `shouldBe` Right 3
        (toList bs >>= traverse readStruct2) `shouldBe` Right [101, 102, 103]
        (traverse (index bs) [0..2] >>= traverse readStruct2) `shouldBe` Right [101, 102, 103]

        vectorLength cs `shouldBe` Right 3
        (toList cs >>= traverse readStruct3) `shouldBe` Right [(104, 105, 106), (107, 108, 109), (110, 111, 112)]
        (traverse (index cs) [0..2] >>= traverse readStruct3) `shouldBe` Right [(104, 105, 106), (107, 108, 109), (110, 111, 112)]

        vectorLength ds `shouldBe` Right 3
        (toList ds >>= traverse readStruct4) `shouldBe` Right [(120, 121, 122, True), (123, 124, 125, False), (126, 127, 128, True)]
        (traverse (index ds) [0..2] >>= traverse readStruct4) `shouldBe` Right [(120, 121, 122, True), (123, 124, 125, False), (126, 127, 128, True)]

      it "empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfStructs
              (Just (vector' [])) (Just (vector' [])) (Just (vector' [])) (Just (vector' []))

        as <- fromRightJust $ vectorOfStructsAs x
        bs <- fromRightJust $ vectorOfStructsBs x
        cs <- fromRightJust $ vectorOfStructsCs x
        ds <- fromRightJust $ vectorOfStructsDs x

        vectorLength as `shouldBe` Right 0
        (toList as >>= traverse readStruct1) `shouldBe` Right []

        vectorLength bs `shouldBe` Right 0
        (toList bs >>= traverse readStruct2) `shouldBe` Right []

        vectorLength cs `shouldBe` Right 0
        (toList cs >>= traverse readStruct3) `shouldBe` Right []

        vectorLength ds `shouldBe` Right 0
        (toList ds >>= traverse readStruct4) `shouldBe` Right []

      it "missing" $ do
        x <- fromRight $ decode @VectorOfStructs $ encode $ vectorOfStructs Nothing Nothing Nothing Nothing
        vectorOfStructsAs x `shouldBeRightAnd` isNothing
        vectorOfStructsBs x `shouldBeRightAnd` isNothing
        vectorOfStructsCs x `shouldBeRightAnd` isNothing
        vectorOfStructsDs x `shouldBeRightAnd` isNothing

    describe "VectorOfUnions" $ do
      it "non empty" $ do
        let
          shouldBeSword x (Union (WeaponSword s)) = swordX s `shouldBe` Right (Just x)
          shouldBeSword _ _                       = unexpectedUnionType

          shouldBeAxe y (Union (WeaponAxe s)) = axeY s `shouldBe` Right y
          shouldBeAxe _ _                     = unexpectedUnionType

          shouldBeNone UnionNone = pure ()
          shouldBeNone _         = unexpectedUnionType

        x <- fromRight $ decode $ encode $ vectorOfUnions
          (Just $ vector'
            [ weaponSword (sword (Just "hi"))
            , none
            , weaponAxe (axe (Just 98))
            ]
          )
          (vector'
            [ weaponSword (sword (Just "hi2"))
            , none
            , weaponAxe (axe (Just 100))
            ]
          )

        Just xs <- fromRight $ vectorOfUnionsXs x
        vectorLength xs `shouldBe` Right 3
        length <$> toList xs `shouldBe` Right 3
        xs `index` 0 `shouldBeRightAndExpect` shouldBeSword "hi"
        xs `index` 1 `shouldBeRightAndExpect` shouldBeNone
        xs `index` 2 `shouldBeRightAndExpect` shouldBeAxe 98
        (toList xs <&> (!! 0)) `shouldBeRightAndExpect` shouldBeSword "hi"
        (toList xs <&> (!! 1)) `shouldBeRightAndExpect` shouldBeNone
        (toList xs <&> (!! 2)) `shouldBeRightAndExpect` shouldBeAxe 98

        xsReq <- fromRight $ vectorOfUnionsXsReq x
        vectorLength xsReq `shouldBe` Right 3
        length <$> toList xsReq `shouldBe` Right 3
        xsReq `index` 0 `shouldBeRightAndExpect` shouldBeSword "hi2"
        xsReq `index` 1 `shouldBeRightAndExpect` shouldBeNone
        xsReq `index` 2 `shouldBeRightAndExpect` shouldBeAxe 100
        (toList xsReq <&> (!! 0)) `shouldBeRightAndExpect` shouldBeSword "hi2"
        (toList xsReq <&> (!! 1)) `shouldBeRightAndExpect` shouldBeNone
        (toList xsReq <&> (!! 2)) `shouldBeRightAndExpect` shouldBeAxe 100

      it "empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfUnions (Just (vector' [])) (vector' [])

        Just xs <- fromRight $ vectorOfUnionsXs x
        vectorLength xs `shouldBe` Right 0
        length <$> toList xs `shouldBe` Right 0

        xsReq <- fromRight $ vectorOfUnionsXsReq x
        vectorLength xsReq `shouldBe` Right 0
        length <$> toList xsReq `shouldBe` Right 0

      it "missing" $ do
        x <- fromRight $ decode $ encode $ vectorOfUnions Nothing (vector' [])
        vectorOfUnionsXs x `shouldBeRightAnd` isNothing
        (vectorOfUnionsXsReq x >>= vectorLength) `shouldBe` Right 0

      it "throws when union type vector is present, but union value vector is missing" $ do
        x <- fromRight $ decode $ encode $ writeTable @VectorOfUnions
          [ writeVectorTableField $ vector' @Word8 []
          , missing
          , missing
          , missing
          , writeVectorTableField $ vector' @Word8 []
          ]
        vectorOfUnionsXs x `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        vectorOfUnionsXsReq x `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."

    describe "ScalarsWithDefaults" $ do
      let runTest buffer = do
            x <- fromRight $ decode $ encode buffer

            scalarsWithDefaultsA x `shouldBe` Right 8
            scalarsWithDefaultsB x `shouldBe` Right 16
            scalarsWithDefaultsC x `shouldBe` Right 32
            scalarsWithDefaultsD x `shouldBe` Right 64
            scalarsWithDefaultsE x `shouldBe` Right (-1)
            scalarsWithDefaultsF x `shouldBe` Right (-2)
            scalarsWithDefaultsG x `shouldBe` Right (-4)
            scalarsWithDefaultsH x `shouldBe` Right (-8)
            scalarsWithDefaultsI x `shouldBe` Right 3.9
            scalarsWithDefaultsJ x `shouldBe` Right (-2.3e10)
            scalarsWithDefaultsK x `shouldBe` Right True
            scalarsWithDefaultsL x `shouldBe` Right False
            toColor <$> scalarsWithDefaultsM x `shouldBe` Right (Just ColorBlue)
            toColor <$> scalarsWithDefaultsN x `shouldBe` Right (Just ColorGray)

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
      x <- fromRight $ decode $ encode $ deprecatedFields (Just 1) (Just 2) (Just 3) (Just 4)

      deprecatedFieldsA x `shouldBe` Right 1
      deprecatedFieldsC x `shouldBe` Right 2
      deprecatedFieldsE x `shouldBe` Right 3
      deprecatedFieldsG x `shouldBe` Right 4

    it "RequiredFields" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) struct1X struct1Y struct1Z
      x <- fromRight $ decode $ encode $ requiredFields
        "hello"
        (struct1 11 22 33)
        (axe (Just 44))
        (weaponSword (sword (Just "a")))
        (vector' [55, 66])

      requiredFieldsA x `shouldBe` Right "hello"
      (requiredFieldsB x >>= readStruct1) `shouldBe` Right (11, 22, 33)
      (requiredFieldsC x >>= axeY) `shouldBe` Right 44
      requiredFieldsD x `shouldBeRightAndExpect` \case
        Union (WeaponSword x) -> swordX x `shouldBe` Right (Just "a")
        _                     -> unexpectedUnionType
      (requiredFieldsE x >>= toList) `shouldBe` Right [55, 66]


unexpectedUnionType :: HasCallStack => Expectation
unexpectedUnionType = expectationFailure "Unexpected union type"
