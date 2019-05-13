{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module FlatBuffers.RoundTripSpec where

import           Control.Applicative        ( liftA3 )
import           Control.Monad              ( when )

import           Data.Functor               ( (<&>) )
import qualified Data.List                  as L
import           Data.Maybe                 ( isNothing )
import           Data.Text                  ( Text )
import           Data.Word

import           Examples.HandWritten

import qualified FlatBuffers.Internal.Write as W
import           FlatBuffers.Read
import           FlatBuffers.Write

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

        getPrimitives'a x `shouldBe` Right maxBound
        getPrimitives'b x `shouldBe` Right maxBound
        getPrimitives'c x `shouldBe` Right maxBound
        getPrimitives'd x `shouldBe` Right maxBound
        getPrimitives'e x `shouldBe` Right maxBound
        getPrimitives'f x `shouldBe` Right maxBound
        getPrimitives'g x `shouldBe` Right maxBound
        getPrimitives'h x `shouldBe` Right maxBound
        getPrimitives'i x `shouldBe` Right 1234.56
        getPrimitives'j x `shouldBe` Right 2873242.82782
        getPrimitives'k x `shouldBe` Right True
        getPrimitives'l x `shouldBe` Right (Just "hi 👬 bye")


      it "missing" $ do
        x <- fromRight $ decode @Primitives $ encodeWithFileIdentifier $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
        getPrimitives'a x `shouldBe` Right 0
        getPrimitives'b x `shouldBe` Right 0
        getPrimitives'c x `shouldBe` Right 0
        getPrimitives'd x `shouldBe` Right 0
        getPrimitives'e x `shouldBe` Right 0
        getPrimitives'f x `shouldBe` Right 0
        getPrimitives'g x `shouldBe` Right 0
        getPrimitives'h x `shouldBe` Right 0
        getPrimitives'i x `shouldBe` Right 0
        getPrimitives'j x `shouldBe` Right 0
        getPrimitives'k x `shouldBe` Right False
        getPrimitives'l x `shouldBe` Right Nothing

    describe "Enums" $ do
      let readStructWithEnum = (liftA3 . liftA3) (,,) getStructWithEnum'x (fmap toColor <$> getStructWithEnum'y) getStructWithEnum'z
      it "present" $ do
        x <- fromRight $ decode $ encode $ enums
          (Just (fromColor ColorGray))
          (Just (structWithEnum 11 (fromColor ColorRed) 22))
          (Just [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen])
          (Just [structWithEnum 33 (fromColor ColorRed) 44, structWithEnum 55 (fromColor ColorGreen) 66])

        toColor <$> getEnums'x x `shouldBe` Right (Just ColorGray)
        (getEnums'y x >>= traverse readStructWithEnum) `shouldBe` Right (Just (11, Just ColorRed, 22))
        (getEnums'xs x >>= traverse toList) `shouldBe` Right (Just [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen])
        (getEnums'ys x >>= traverse toList >>= traverse (traverse readStructWithEnum)) `shouldBe` Right (Just [(33, Just ColorRed, 44), (55, Just ColorGreen, 66)])

      it "present with defaults" $ do
        x <- fromRight $ decode @Enums $ encode $ enums (Just (fromColor ColorGreen)) Nothing Nothing Nothing

        toColor <$> getEnums'x x `shouldBe` Right (Just ColorGreen)
        getEnums'y x `shouldBeRightAnd` isNothing
        getEnums'xs x `shouldBeRightAnd` isNothing
        getEnums'ys x `shouldBeRightAnd` isNothing

      it "missing" $ do
        x <- fromRight $ decode @Enums $ encode $ enums Nothing Nothing Nothing Nothing

        toColor <$> getEnums'x x `shouldBe` Right (Just ColorGreen)
        getEnums'y x `shouldBeRightAnd` isNothing
        getEnums'xs x `shouldBeRightAnd` isNothing
        getEnums'ys x `shouldBeRightAnd` isNothing

    describe "Structs" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) getStruct1'x getStruct1'y getStruct1'z
      let readStruct2 = getStruct2'x
      let readStruct3 = (liftA3 . liftA3) (,,) (getStruct2'x . getStruct3'x) getStruct3'y getStruct3'z
      let readStruct4 = (liftA4 . liftA4) (,,,) (getStruct2'x . getStruct4'w) getStruct4'x getStruct4'y getStruct4'z
      it "present" $ do
        root <- fromRight $ decode $ encode $ structs
                (Just (struct1 1 2 3))
                (Just (struct2 11))
                (Just (struct3 22 33 44))
                (Just (struct4 55 66 77 True))

        s1 <- fromRightJust $ getStructs'a root
        s2 <- fromRightJust $ getStructs'b root
        s3 <- fromRightJust $ getStructs'c root
        s4 <- fromRightJust $ getStructs'd root

        readStruct1 s1 `shouldBe` Right (1, 2, 3)
        readStruct2 s2 `shouldBe` Right 11
        readStruct3 s3 `shouldBe` Right (22, 33, 44)
        readStruct4 s4 `shouldBe` Right (55, 66, 77, True)

      it "missing" $ do
        root <- fromRight $ decode $ encode $ structs Nothing Nothing Nothing Nothing

        getStructs'a root `shouldBeRightAnd` isNothing
        getStructs'b root `shouldBeRightAnd` isNothing
        getStructs'c root `shouldBeRightAnd` isNothing
        getStructs'd root `shouldBeRightAnd` isNothing

    describe "Nested tables" $ do
      it "present" $ do
        root <- fromRight $ decode $ encode $ nestedTables (Just (table1 (Just (table2 (Just 11))) (Just 22)))

        t1 <- fromRightJust $ getNestedTables'x root
        t2 <- fromRightJust $ getTable1'x t1

        getTable1'y t1 `shouldBe` Right 22
        getTable2'x t2 `shouldBe` Right 11

      it "missing table2" $ do
        root <- fromRight $ decode $ encode $ nestedTables (Just (table1 Nothing (Just 22)))

        t1 <- fromRightJust $ getNestedTables'x root
        getTable1'x t1 `shouldBeRightAnd` isNothing
        getTable1'y t1 `shouldBe` Right 22

      it "missing table1" $ do
        root <- fromRight $ decode $ encode $ nestedTables Nothing

        getNestedTables'x root `shouldBeRightAnd` isNothing

    describe "Union" $ do
      it "present" $ do
        x <- fromRight $ decode $ encode $ tableWithUnion (Just (weapon (sword (Just "hi"))))
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          Union (Weapon'Sword x) -> getSword'x x `shouldBe` Right (Just "hi")
          _                      -> unexpectedUnionType

        x <- fromRight $ decode $ encode $ tableWithUnion (Just (weapon (axe (Just maxBound))))
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          Union (Weapon'Axe x) -> getAxe'y x `shouldBe` Right maxBound
          _                    -> unexpectedUnionType

        x <- fromRight $ decode $ encode $ tableWithUnion (Just none)
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "missing" $ do
        x <- fromRight $ decode $ encode $ tableWithUnion Nothing
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "throws when union type is present, but union value is missing" $ do
        x <- fromRight $ decode $ encode $ writeTable @TableWithUnion [inline word8 1]
        getTableWithUnion'uni x `shouldBeLeft` MalformedBuffer "Union: 'union type' found but 'union value' is missing."

    describe "Vectors" $ do
      let Right nonEmptyVecs = decode $ encode $ vectors
            (Just [minBound, 0, maxBound])
            (Just [minBound, 0, maxBound])
            (Just [minBound, 0, maxBound])
            (Just [minBound, 0, maxBound])
            (Just [minBound, 0, maxBound])
            (Just [minBound, 0, maxBound])
            (Just [minBound, 0, maxBound])
            (Just [minBound, 0, maxBound])
            (Just [-12e9, 0, 3.33333333333333333333])
            (Just [-12e98, 0, 3.33333333333333333333])
            (Just [True, False, True])
            (Just ["hi 👬 bye", "", "world"])

      let Right emptyVecs = decode $ encode $ vectors
            (Just []) (Just []) (Just []) (Just [])
            (Just []) (Just []) (Just []) (Just [])
            (Just []) (Just []) (Just []) (Just [])

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

      describe "word8 vector"  $ testPrimVector getVectors'a [minBound, 0, maxBound]
      describe "word16 vector" $ testPrimVector getVectors'b [minBound, 0, maxBound]
      describe "word32 vector" $ testPrimVector getVectors'c [minBound, 0, maxBound]
      describe "word64 vector" $ testPrimVector getVectors'd [minBound, 0, maxBound]
      describe "int8 vector"   $ testPrimVector getVectors'e [minBound, 0, maxBound]
      describe "int16 vector"  $ testPrimVector getVectors'f [minBound, 0, maxBound]
      describe "int32 vector"  $ testPrimVector getVectors'g [minBound, 0, maxBound]
      describe "int64 vector"  $ testPrimVector getVectors'h [minBound, 0, maxBound]
      describe "float vector"  $ testPrimVector getVectors'i [-12e9, 0, 3.33333333333333333333]
      describe "double vector" $ testPrimVector getVectors'j [-12e98, 0, 3.33333333333333333333]
      describe "bool vector"   $ testPrimVector getVectors'k [True, False, True]
      describe "string vector" $ testPrimVector getVectors'l ["hi 👬 bye", "", "world"]

    describe "VectorOfTables" $ do
      it "non empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfTables (Just
          [ axe (Just minBound)
          , axe (Just 0)
          , axe (Just maxBound)
          ])

        Just xs <- fromRight $ getVectorOfTables'xs x
        vectorLength xs `shouldBe` Right 3
        (toList xs >>= traverse getAxe'y) `shouldBe` Right [minBound, 0, maxBound]
        (traverse (index xs) [0..2] >>= traverse getAxe'y) `shouldBe` Right [minBound, 0, maxBound]

      it "empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfTables (Just [])

        xs <- fromRightJust $ getVectorOfTables'xs x
        vectorLength xs `shouldBe` Right 0
        (toList xs >>= traverse getAxe'y) `shouldBe` Right []

      it "missing" $ do
        x <- fromRight $ decode $ encode $ vectorOfTables Nothing
        getVectorOfTables'xs x `shouldBeRightAnd` isNothing

    describe "VectorOfStructs" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) getStruct1'x getStruct1'y getStruct1'z
      let readStruct2 = getStruct2'x
      let readStruct3 = (liftA3 . liftA3) (,,) (getStruct2'x . getStruct3'x) getStruct3'y getStruct3'z
      let readStruct4 = (liftA4 . liftA4) (,,,) (getStruct2'x . getStruct4'w) getStruct4'x getStruct4'y getStruct4'z

      it "non empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfStructs
          (Just [struct1 1 2 3, struct1 4 5 6])
          (Just [struct2 101, struct2 102, struct2 103])
          (Just [struct3 104 105 106, struct3 107 108 109, struct3 110 111 112])
          (Just [struct4 120 121 122 True, struct4 123 124 125 False, struct4 126 127 128 True])

        as <- fromRightJust $ getVectorOfStructs'as x
        bs <- fromRightJust $ getVectorOfStructs'bs x
        cs <- fromRightJust $ getVectorOfStructs'cs x
        ds <- fromRightJust $ getVectorOfStructs'ds x

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
        x <- fromRight $ decode $ encode $ vectorOfStructs (Just []) (Just []) (Just []) (Just [])

        as <- fromRightJust $ getVectorOfStructs'as x
        bs <- fromRightJust $ getVectorOfStructs'bs x
        cs <- fromRightJust $ getVectorOfStructs'cs x
        ds <- fromRightJust $ getVectorOfStructs'ds x

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
        getVectorOfStructs'as x `shouldBeRightAnd` isNothing
        getVectorOfStructs'bs x `shouldBeRightAnd` isNothing
        getVectorOfStructs'cs x `shouldBeRightAnd` isNothing
        getVectorOfStructs'ds x `shouldBeRightAnd` isNothing

    describe "VectorOfUnions" $ do
      it "non empty" $ do
        let
          shouldBeSword swordX (Union (Weapon'Sword s)) = getSword'x s `shouldBe` Right (Just swordX)
          shouldBeSword _ _                             = unexpectedUnionType

          shouldBeAxe axeY (Union (Weapon'Axe s)) = getAxe'y s `shouldBe` Right axeY
          shouldBeAxe _ _                         = unexpectedUnionType

          shouldBeNone UnionNone = pure ()
          shouldBeNone _         = unexpectedUnionType

        x <- fromRight $ decode $ encode $ vectorOfUnions
          (Just
            [ weapon (sword (Just "hi"))
            , none
            , weapon (axe (Just 98))
            ]
          )
          [ weapon (sword (Just "hi2"))
          , none
          , weapon (axe (Just 100))
          ]

        Just xs <- fromRight $ getVectorOfUnions'xs x
        vectorLength xs `shouldBe` Right 3
        length <$> toList xs `shouldBe` Right 3
        xs `index` 0 `shouldBeRightAndExpect` shouldBeSword "hi"
        xs `index` 1 `shouldBeRightAndExpect` shouldBeNone
        xs `index` 2 `shouldBeRightAndExpect` shouldBeAxe 98
        (toList xs <&> (!! 0)) `shouldBeRightAndExpect` shouldBeSword "hi"
        (toList xs <&> (!! 1)) `shouldBeRightAndExpect` shouldBeNone
        (toList xs <&> (!! 2)) `shouldBeRightAndExpect` shouldBeAxe 98

        xsReq <- fromRight $ getVectorOfUnions'xsReq x
        vectorLength xsReq `shouldBe` Right 3
        length <$> toList xsReq `shouldBe` Right 3
        xsReq `index` 0 `shouldBeRightAndExpect` shouldBeSword "hi2"
        xsReq `index` 1 `shouldBeRightAndExpect` shouldBeNone
        xsReq `index` 2 `shouldBeRightAndExpect` shouldBeAxe 100
        (toList xsReq <&> (!! 0)) `shouldBeRightAndExpect` shouldBeSword "hi2"
        (toList xsReq <&> (!! 1)) `shouldBeRightAndExpect` shouldBeNone
        (toList xsReq <&> (!! 2)) `shouldBeRightAndExpect` shouldBeAxe 100

      it "empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfUnions (Just []) [ ]

        Just xs <- fromRight $ getVectorOfUnions'xs x
        vectorLength xs `shouldBe` Right 0
        length <$> toList xs `shouldBe` Right 0

        xsReq <- fromRight $ getVectorOfUnions'xsReq x
        vectorLength xsReq `shouldBe` Right 0
        length <$> toList xsReq `shouldBe` Right 0

      it "missing" $ do
        x <- fromRight $ decode $ encode $ vectorOfUnions Nothing []
        getVectorOfUnions'xs x `shouldBeRightAnd` isNothing
        (getVectorOfUnions'xsReq x >>= vectorLength) `shouldBe` Right 0

      it "throws when union type vector is present, but union value vector is missing" $ do
        x <- fromRight $ decode $ encode $ writeTable @VectorOfUnions
          [ (writeVector . inline) word8 []
          , W.missing
          , W.missing
          , W.missing
          , (writeVector . inline) word8 []
          , W.missing
          ]
        getVectorOfUnions'xs x `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        getVectorOfUnions'xsReq x `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."

    describe "ScalarsWithDefaults" $ do
      let runTest buffer = do
            x <- fromRight $ decode $ encode $ buffer

            getScalarsWithDefaults'a x `shouldBe` Right 8
            getScalarsWithDefaults'b x `shouldBe` Right 16
            getScalarsWithDefaults'c x `shouldBe` Right 32
            getScalarsWithDefaults'd x `shouldBe` Right 64
            getScalarsWithDefaults'e x `shouldBe` Right (-1)
            getScalarsWithDefaults'f x `shouldBe` Right (-2)
            getScalarsWithDefaults'g x `shouldBe` Right (-4)
            getScalarsWithDefaults'h x `shouldBe` Right (-8)
            getScalarsWithDefaults'i x `shouldBe` Right 3.9
            getScalarsWithDefaults'j x `shouldBe` Right (-2.3e10)
            getScalarsWithDefaults'k x `shouldBe` Right True
            getScalarsWithDefaults'l x `shouldBe` Right False
            toColor <$> getScalarsWithDefaults'm x `shouldBe` Right (Just ColorBlue)
            toColor <$> getScalarsWithDefaults'n x `shouldBe` Right (Just ColorGray)

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

      getDeprecatedFields'a x `shouldBe` Right 1
      getDeprecatedFields'c x `shouldBe` Right 2
      getDeprecatedFields'e x `shouldBe` Right 3
      getDeprecatedFields'g x `shouldBe` Right 4

    it "RequiredFields" $ do
      let readStruct1 = (liftA3 . liftA3) (,,) getStruct1'x getStruct1'y getStruct1'z
      x <- fromRight $ decode $ encode $ requiredFields
        "hello"
        (struct1 11 22 33)
        (axe (Just 44))
        (weapon (sword (Just "a")))
        [55, 66]

      getRequiredFields'a x `shouldBe` Right "hello"
      (getRequiredFields'b x >>= readStruct1) `shouldBe` Right (11, 22, 33)
      (getRequiredFields'c x >>= getAxe'y) `shouldBe` Right 44
      getRequiredFields'd x `shouldBeRightAndExpect` \case
        Union (Weapon'Sword x) -> getSword'x x `shouldBe` Right (Just "a")
        _                      -> unexpectedUnionType
      (getRequiredFields'e x >>= toList) `shouldBe` Right [55, 66]



unexpectedUnionType = expectationFailure "Unexpected union type"
