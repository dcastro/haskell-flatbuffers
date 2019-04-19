{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FlatBuffers.RoundTripSpec where

import           Control.Applicative        ( liftA3 )
import           Control.Monad              ( forM )

import           Data.Maybe                 ( fromJust, isNothing )
import           Data.Text                  ( Text )
import           Data.Word

import           Examples.HandWritten

import qualified FlatBuffers.Internal.Write as W
import           FlatBuffers.Read
import           FlatBuffers.Write

import           Test.Hspec

spec :: Spec
spec =
  describe "Round Trip" $ do
    describe "Primitives" $ do
      it "writes file identifier to buffer" $ do
        let bs = encodeWithFileIdentifier $ primitives
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just 1234.56) (Just 2873242.82782) (Just True)

        checkFileIdentifier @Primitives bs `shouldBe` True
        
      it "present" $ do
        x <- decode @Primitives $ encodeWithFileIdentifier $ primitives
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just 1234.56) (Just 2873242.82782) (Just True)
        getPrimitives'a x `shouldBe` Just maxBound
        getPrimitives'b x `shouldBe` Just maxBound
        getPrimitives'c x `shouldBe` Just maxBound
        getPrimitives'd x `shouldBe` Just maxBound
        getPrimitives'e x `shouldBe` Just maxBound
        getPrimitives'f x `shouldBe` Just maxBound
        getPrimitives'g x `shouldBe` Just maxBound
        getPrimitives'h x `shouldBe` Just maxBound
        getPrimitives'i x `shouldBe` Just 1234.56
        getPrimitives'j x `shouldBe` Just 2873242.82782
        getPrimitives'k x `shouldBe` Just True

      it "missing" $ do
        x <- decode @Primitives $ encodeWithFileIdentifier $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing
        getPrimitives'a x `shouldBe` Just 1
        getPrimitives'b x `shouldBe` Just 1
        getPrimitives'c x `shouldBe` Just 1
        getPrimitives'd x `shouldBe` Just 1
        getPrimitives'e x `shouldBe` Just 1
        getPrimitives'f x `shouldBe` Just 1
        getPrimitives'g x `shouldBe` Just 1
        getPrimitives'h x `shouldBe` Just 1
        getPrimitives'i x `shouldBe` Just 1
        getPrimitives'j x `shouldBe` Just 1
        getPrimitives'k x `shouldBe` Just False

    describe "Enums" $ do
      let readStructWithEnum = (liftA3 . liftA3) (,,) getStructWithEnum'x (fmap toColor <$> getStructWithEnum'y) getStructWithEnum'z
      it "present" $ do
        x <- decode $ encode $ enums
          (Just (fromColor ColorGray))
          (Just (structWithEnum 11 (fromColor ColorRed) 22))
          [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen]
          (Just [structWithEnum 33 (fromColor ColorRed) 44, structWithEnum 55 (fromColor ColorGreen) 66])

        toColor <$> getEnums'x x `shouldBe` Just (Just ColorGray)
        (getEnums'y x >>= traverse readStructWithEnum) `shouldBe` Just (Just (11, Just ColorRed, 22))
        (getEnums'xs x >>= toList) `shouldBe` Just [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen]
        (getEnums'ys x >>= traverse toList >>= traverse (traverse readStructWithEnum)) `shouldBe` Just (Just [(33, Just ColorRed, 44), (55, Just ColorGreen, 66)])

      it "missing" $ do
        x <- decode @Enums $ encode $ enums Nothing Nothing [] Nothing

        toColor <$> getEnums'x x `shouldBe` Just (Just ColorBlue)
        getEnums'y x >>= \mb -> isNothing mb `shouldBe` True
        (getEnums'xs x >>= toList) `shouldBe` Just []
        getEnums'ys x >>= \mb -> isNothing mb `shouldBe` True

    describe "Union" $ do
      it "present" $ do
        x <- decode $ encode $ tableWithUnion (Just (weapon (sword (Just "hi")))) none
        getTableWithUnion'uni x >>= \case
          Union (Weapon'Sword x) -> getSword'x x `shouldBe` Just (Just "hi")
          _                      -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just (weapon (axe (Just maxBound)))) none
        getTableWithUnion'uni x >>= \case
          Union (Weapon'Axe x) -> getAxe'y x `shouldBe` Just maxBound
          _                    -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just none) none
        getTableWithUnion'uni x >>= \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "missing" $ do
        x <- decode $ encode $ tableWithUnion Nothing none
        getTableWithUnion'uni x >>= \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType

      it "throws when union type is present, but union value is missing" $ do
        let union = writeUnion 1 (writeTable [w @Text "hello"])
        x <- decode $ encode $ writeTable @TableWithUnion [wType union]
        getTableWithUnion'uni x `shouldThrow` \err -> err == MalformedBuffer "Union: 'union type' found but 'union value' is missing."

    describe "VectorOfUnions" $ do
      it "present" $ do
        x <- decode $ encode $ vectorOfUnions
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

        Just xs <- getVectorOfUnions'xs x
        vectorLength xs `shouldBe` 3
        xs `index` 0 >>= \case
          Union (Weapon'Sword x) -> getSword'x x `shouldBe` Just (Just "hi")
          _                      -> unexpectedUnionType
        xs `index` 1 >>= \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType
        xs `index` 2 >>= \case
          Union (Weapon'Axe x) -> getAxe'y x `shouldBe` Just 98
          _                    -> unexpectedUnionType
        xs `index` 3 `shouldThrow` \err -> err == VectorIndexOutOfBounds 3 3

        xsReq <- getVectorOfUnions'xsReq x
        vectorLength xsReq `shouldBe` 3
        xsReq `index` 0 >>= \case
          Union (Weapon'Sword x) -> getSword'x x `shouldBe` Just (Just "hi2")
          _                      -> unexpectedUnionType
        xsReq `index` 1 >>= \case
          UnionNone -> pure ()
          _         -> unexpectedUnionType
        xsReq `index` 2 >>= \case
          Union (Weapon'Axe x) -> getAxe'y x `shouldBe` Just 100
          _                    -> unexpectedUnionType
        xsReq `index` 3 `shouldThrow` \err -> err == VectorIndexOutOfBounds 3 3

      it "missing" $ do
        x <- decode $ encode $ vectorOfUnions Nothing []
        getVectorOfUnions'xs x >>= \mb -> isNothing mb `shouldBe` True
        vectorLength <$> getVectorOfUnions'xsReq x `shouldBe` Just 0

      it "throws when union type vector is present, but union value vector is missing" $ do
        x <- decode $ encode $ writeTable @VectorOfUnions
          [ w @[Word8] []
          , W.missing
          , w @[Word8] []
          , W.missing
          ]
        getVectorOfUnions'xs x `shouldThrow` \err -> err == MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        getVectorOfUnions'xsReq x `shouldThrow` \err -> err == MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."

    describe "VectorOfStructs" $ do
      let getBytes = (liftA3 . liftA3) (,,) getThreeBytes'a getThreeBytes'b getThreeBytes'c
      it "present" $ do
        x <- decode $ encode $ vectorOfStructs (Just
          [ threeBytes 1 2 3
          , threeBytes 4 5 6
          ])
        Just xs <- getVectorOfStructs'xs x
        (toList xs >>= traverse getBytes) `shouldBe` Just [(1,2,3), (4,5,6)]
      it "missing" $ do
        x <- decode @VectorOfStructs $ encode $ vectorOfStructs Nothing
        getVectorOfStructs'xs x >>= \mb -> isNothing mb `shouldBe` True

    describe "Align" $ do
      it "present" $ do
        root <- decode $ encode $ alignT
                (Just (align1 11))
                (Just (align2 22 33 44))
                (Just [align1 101, align1 102, align1 103])
                (Just [align2 104 105 106, align2 107 108 109, align2 110 111 112])

        Just a1 <- getAlignT'x root
        Just a2 <- getAlignT'y root
        a1s <- getAlignT'xs root >>= toList . fromJust
        a2s <- getAlignT'ys root >>= toList . fromJust

        getAlign1'x a1 `shouldBe` Just 11

        getAlign1'x (getAlign2'x a2) `shouldBe` Just 22
        getAlign2'y a2 `shouldBe` Just 33
        getAlign2'z a2 `shouldBe` Just 44

        traverse getAlign1'x a1s `shouldBe` Just [101, 102, 103]

        forM a2s (\a2 -> (,,) <$> getAlign1'x (getAlign2'x a2) <*> getAlign2'y a2 <*> getAlign2'z a2)
          `shouldBe` Just [(104, 105, 106), (107, 108, 109), (110, 111, 112)]


      it "missing" $ do
        root <- decode $ encode $ alignT Nothing Nothing Nothing Nothing

        getAlignT'x root >>= \mb -> isNothing mb `shouldBe` True
        getAlignT'y root >>= \mb -> isNothing mb `shouldBe` True
        getAlignT'xs root >>= \mb -> isNothing mb `shouldBe` True
        getAlignT'ys root >>= \mb -> isNothing mb `shouldBe` True

unexpectedUnionType = expectationFailure "Unexpected union type"
