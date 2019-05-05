{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FlatBuffers.RoundTripSpec where

import           Control.Applicative        ( liftA3 )
import           Control.Monad              ( forM, when )

import           Data.Functor               ( (<&>) )
import qualified Data.List                  as L
import           Data.Maybe                 ( fromJust, isNothing )
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
        let bs = encodeWithFileIdentifier $ primitives
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just 1234.56) (Just 2873242.82782) (Just True) (Just "hi 👬 bye")

        checkFileIdentifier @Primitives bs `shouldBe` True

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
        getPrimitives'a x `shouldBe` Right 1
        getPrimitives'b x `shouldBe` Right 1
        getPrimitives'c x `shouldBe` Right 1
        getPrimitives'd x `shouldBe` Right 1
        getPrimitives'e x `shouldBe` Right 1
        getPrimitives'f x `shouldBe` Right 1
        getPrimitives'g x `shouldBe` Right 1
        getPrimitives'h x `shouldBe` Right 1
        getPrimitives'i x `shouldBe` Right 1
        getPrimitives'j x `shouldBe` Right 1
        getPrimitives'k x `shouldBe` Right False
        getPrimitives'l x `shouldBe` Right Nothing

    describe "Enums" $ do
      let readStructWithEnum = (liftA3 . liftA3) (,,) getStructWithEnum'x (fmap toColor <$> getStructWithEnum'y) getStructWithEnum'z
      it "present" $ do
        x <- fromRight $ decode $ encode $ enums
          (Just (fromColor ColorGray))
          (Just (structWithEnum 11 (fromColor ColorRed) 22))
          [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen]
          (Just [structWithEnum 33 (fromColor ColorRed) 44, structWithEnum 55 (fromColor ColorGreen) 66])

        toColor <$> getEnums'x x `shouldBe` Right (Just ColorGray)
        (getEnums'y x >>= traverse readStructWithEnum) `shouldBe` Right (Just (11, Just ColorRed, 22))
        (getEnums'xs x >>= toList) `shouldBe` Right [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen]
        (getEnums'ys x >>= traverse toList >>= traverse (traverse readStructWithEnum)) `shouldBe` Right (Just [(33, Just ColorRed, 44), (55, Just ColorGreen, 66)])

      it "missing" $ do
        x <- fromRight $ decode @Enums $ encode $ enums Nothing Nothing [] Nothing

        toColor <$> getEnums'x x `shouldBe` Right (Just ColorBlue)
        getEnums'y x `shouldBeRightAnd` isNothing
        (getEnums'xs x >>= toList) `shouldBe` Right []
        getEnums'ys x `shouldBeRightAnd` isNothing

    describe "Union" $ do
      it "present" $ do
        x <- fromRight $ decode $ encode $ tableWithUnion (Just (weapon (sword (Just "hi")))) none
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          Union (Weapon'Sword x) -> getSword'x x `shouldBe` Right (Just "hi")
          _                      -> unexpectedUnionType

        x <- fromRight $ decode $ encode $ tableWithUnion (Just (weapon (axe (Just maxBound)))) none
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          Union (Weapon'Axe x) -> getAxe'y x `shouldBe` Right maxBound
          _                    -> unexpectedUnionType

        x <- fromRight $ decode $ encode $ tableWithUnion (Just none) none
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "missing" $ do
        x <- fromRight $ decode $ encode $ tableWithUnion Nothing none
        getTableWithUnion'uni x `shouldBeRightAndExpect` \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "throws when union type is present, but union value is missing" $ do
        x <- fromRight $ decode $ encode $ writeTable @TableWithUnion [inline word8 1]
        getTableWithUnion'uni x `shouldBeLeft` MalformedBuffer "Union: 'union type' found but 'union value' is missing."

    describe "Vectors" $ do
      let
        testPrimVector :: (VectorElement a, Show a, Eq a) => [a] -> Maybe (Vector a) -> Expectation
        testPrimVector list Nothing = expectationFailure "Expected 'Just', got 'Nothing'"
        testPrimVector list (Just vec) = do
          vectorLength vec `shouldBe` Right (L.genericLength list)
          toList vec `shouldBe` Right list
          when (not $ null list) $
            traverse (\i -> vec `index` i) [0 .. L.genericLength list - 1] `shouldBe` Right list

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


      describe "word8 vector"  $ do
        it "non empty" $ getVectors'a nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'a emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'a missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "word16 vector" $ do
        it "non empty" $ getVectors'b nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'b emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'b missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "word32 vector" $ do
        it "non empty" $ getVectors'c nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'c emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'c missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "word64 vector" $ do
        it "non empty" $ getVectors'd nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'd emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'd missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "int8 vector"   $ do
        it "non empty" $ getVectors'e nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'e emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'e missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "int16 vector"  $ do
        it "non empty" $ getVectors'f nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'f emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'f missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "int32 vector"  $ do
        it "non empty" $ getVectors'g nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'g emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'g missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "int64 vector"  $ do
        it "non empty" $ getVectors'h nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [minBound, 0, maxBound]
        it "empty"     $ getVectors'h emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'h missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "float vector"  $ do
        it "non empty" $ getVectors'i nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [-12e9, 0, 3.33333333333333333333]
        it "empty"     $ getVectors'i emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'i missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "double vector" $ do
        it "non empty" $ getVectors'j nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [-12e98, 0, 3.33333333333333333333]
        it "empty"     $ getVectors'j emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'j missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "bool vector"   $ do
        it "non empty" $ getVectors'k nonEmptyVecs `shouldBeRightAndExpect` testPrimVector [True, False, True]
        it "empty"     $ getVectors'k emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'k missingVecs >>= traverse toList) `shouldBe` Right Nothing
      describe "string vector" $ do
        it "non empty" $ getVectors'l nonEmptyVecs `shouldBeRightAndExpect` testPrimVector ["hi 👬 bye", "", "world"]
        it "empty"     $ getVectors'l emptyVecs    `shouldBeRightAndExpect` testPrimVector []
        it "missing"   $ (getVectors'l missingVecs >>= traverse toList) `shouldBe` Right Nothing

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

        Just xs <- fromRight $ getVectorOfTables'xs x
        vectorLength xs `shouldBe` Right 0
        (toList xs >>= traverse getAxe'y) `shouldBe` Right []

      it "missing" $ do
        x <- fromRight $ decode $ encode $ vectorOfTables Nothing
        getVectorOfTables'xs x `shouldBeRightAnd` isNothing

    describe "VectorOfStructs" $ do
      let getBytes = (liftA3 . liftA3) (,,) getThreeBytes'a getThreeBytes'b getThreeBytes'c
      it "non empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfStructs (Just
          [ threeBytes 1 2 3
          , threeBytes 4 5 6
          ])

        Just xs <- fromRight $ getVectorOfStructs'xs x
        vectorLength xs `shouldBe` Right 2
        (toList xs >>= traverse getBytes) `shouldBe` Right [(1,2,3), (4,5,6)]
        (traverse (index xs) [0..1] >>= traverse getBytes) `shouldBe` Right [(1,2,3), (4,5,6)]

      it "empty" $ do
        x <- fromRight $ decode $ encode $ vectorOfStructs (Just [])

        Just xs <- fromRight $ getVectorOfStructs'xs x
        vectorLength xs `shouldBe` Right 0
        (toList xs >>= traverse getBytes) `shouldBe` Right []

      it "missing" $ do
        x <- fromRight $ decode @VectorOfStructs $ encode $ vectorOfStructs Nothing
        getVectorOfStructs'xs x `shouldBeRightAnd` isNothing

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
          , (writeVector . inline) word8 []
          , W.missing
          ]
        getVectorOfUnions'xs x `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        getVectorOfUnions'xsReq x `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."

    describe "Align" $ do
      it "present" $ do
        root <- fromRight $ decode $ encode $ alignT
                (Just (align1 11))
                (Just (align2 22 33 44))
                (Just [align1 101, align1 102, align1 103])
                (Just [align2 104 105 106, align2 107 108 109, align2 110 111 112])

        Just a1 <- fromRight $ getAlignT'x root
        Just a2 <- fromRight $ getAlignT'y root
        a1s <- fromRight $ getAlignT'xs root >>= toList . fromJust
        a2s <- fromRight $ getAlignT'ys root >>= toList . fromJust

        getAlign1'x a1 `shouldBe` Right 11

        getAlign1'x (getAlign2'x a2) `shouldBe` Right 22
        getAlign2'y a2 `shouldBe` Right 33
        getAlign2'z a2 `shouldBe` Right 44

        traverse getAlign1'x a1s `shouldBe` Right [101, 102, 103]

        forM a2s (\a2 -> (,,) <$> getAlign1'x (getAlign2'x a2) <*> getAlign2'y a2 <*> getAlign2'z a2)
          `shouldBe` Right [(104, 105, 106), (107, 108, 109), (110, 111, 112)]


      it "missing" $ do
        root <- fromRight $ decode $ encode $ alignT Nothing Nothing Nothing Nothing

        getAlignT'x root `shouldBeRightAnd` isNothing
        getAlignT'y root `shouldBeRightAnd` isNothing
        getAlignT'xs root `shouldBeRightAnd` isNothing
        getAlignT'ys root `shouldBeRightAnd` isNothing

unexpectedUnionType = expectationFailure "Unexpected union type"
