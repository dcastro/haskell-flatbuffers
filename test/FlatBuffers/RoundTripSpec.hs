{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module FlatBuffers.RoundTripSpec where

import           Control.Applicative    (liftA3)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import           Data.Word
import           Examples.HandWritten
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
      it "present" $ do
        x <- decode $ encode $ enums (Just Gray)
        getEnums'x x `shouldBe` Just Gray
      it "missing" $ do
        x <- decode @Enums $ encode $ enums Nothing
        getEnums'x x `shouldBe` Just Blue

    describe "Union" $ do
      it "present" $ do
        x <- decode $ encode $ tableWithUnion (Just (union (unionA (Just "hi"))))
        getTableWithUnion'uni x >>= \case
          Union'UnionA x -> getUnionA'x req x `shouldBe` Just "hi"
          _              -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just (union (unionB (Just maxBound))))
        getTableWithUnion'uni x >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just maxBound
          _              -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just none)
        getTableWithUnion'uni x >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType

      it "missing" $ do
        x <- decode $ encode $ tableWithUnion Nothing
        getTableWithUnion'uni x >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType

      it "throws when union type is present, but union value is missing" $ do
        let union = writeUnion 1 (writeTable [w @Text "hello"])
        x <- decode $ encode $ writeTable @TableWithUnion [wType union]
        getTableWithUnion'uni x `shouldThrow` \err -> err == MalformedBuffer "Union: 'union type' found but 'union value' is missing."

    describe "VectorOfUnions" $ do
      it "present" $ do
        x <- decode $ encode $ vectorOfUnions (Just
          [ union (unionA (Just "hi"))
          , none
          , union (unionB (Just 98))
          ])
        xs <- getVectorOfUnions'xs req x
        vectorLength xs `shouldBe` 3
        xs `index` 0 >>= \case
          Union'UnionA x -> getUnionA'x req x `shouldBe` Just "hi"
          _              -> unexpectedUnionType
        xs `index` 1 >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType
        xs `index` 2 >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just 98
          _              -> unexpectedUnionType
        xs `index` 3 `shouldThrow` \err -> err == VectorIndexOutOfBounds 3 3

      it "missing" $ do
        x <- decode $ encode $ vectorOfUnions Nothing
        getVectorOfUnions'xs req x `shouldThrow` \err -> err == MissingField "xs"
        getVectorOfUnions'xs opt x >>= \mb -> isNothing mb `shouldBe` True

      it "throws when union type vector is present, but union value vector is missing" $ do
          x <- decode $ encode $ writeTable @VectorOfUnions [w @[Word8] []]
          getVectorOfUnions'xs opt x `shouldThrow` \err -> err == MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."

    describe "VectorOfStructs" $ do
      let getBytes = (liftA3 . liftA3) (,,) getThreeBytes'a getThreeBytes'b getThreeBytes'c
      it "present" $ do
        x <- decode $ encode $ vectorOfStructs (Just
          [ threeBytes 1 2 3
          , threeBytes 4 5 6
          ])
        xs <- getVectorOfStructs'xs req x
        (toList xs >>= traverse getBytes) `shouldBe` Just [(1,2,3), (4,5,6)]
      it "missing" $ do
        x <- decode @VectorOfStructs $ encode $ vectorOfStructs Nothing
        getVectorOfStructs'xs req x `shouldThrow` \err -> err == MissingField "xs"
        getVectorOfStructs'xs opt x >>= \mb -> isNothing mb `shouldBe` True

unexpectedUnionType = expectationFailure "Unexpected union type"
