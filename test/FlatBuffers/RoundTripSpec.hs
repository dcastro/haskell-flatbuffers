{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module FlatBuffers.RoundTripSpec where

import           Control.Applicative    (liftA3)
import           Control.Monad          (forM)
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
      let readStructWithEnum = (liftA3 . liftA3) (,,) getStructWithEnum'x getStructWithEnum'y getStructWithEnum'z
      it "present" $ do
        x <- decode $ encode $ enums
          (Just Gray)
          (Just (structWithEnum 11 Red 22))
          [Black, Blue, Green]
          (Just [structWithEnum 33 Red 44, structWithEnum 55 Green 66])

        getEnums'x x `shouldBe` Just Gray
        (getEnums'y req x >>= readStructWithEnum) `shouldBe` Just (11, Red, 22)
        (getEnums'xs x >>= toList) `shouldBe` Just [Black, Blue, Green]
        (getEnums'ys req x >>= toList >>= traverse readStructWithEnum) `shouldBe` Just [(33, Red, 44), (55, Green, 66)]

        (getEnums'y opt x >>= traverse readStructWithEnum) `shouldBe` Just (Just (11, Red, 22))
        (getEnums'ys opt x >>= traverse toList >>= traverse (traverse readStructWithEnum)) `shouldBe` Just (Just [(33, Red, 44), (55, Green, 66)])

      it "missing" $ do
        x <- decode @Enums $ encode $ enums Nothing Nothing [] Nothing

        getEnums'x x `shouldBe` Just Blue
        getEnums'y req x `shouldThrow` \err -> err == MissingField "y"
        (getEnums'xs x >>= toList) `shouldBe` Just []
        getEnums'ys req x `shouldThrow` \err -> err == MissingField "ys"

        getEnums'y opt x >>= \mb -> isNothing mb `shouldBe` True
        getEnums'ys opt x >>= \mb -> isNothing mb `shouldBe` True

    describe "Union" $ do
      describe "present" $ do
        it "required mode" $ do
          x <- decode $ encode $ tableWithUnion (Just (union (unionA (Just "hi"))))
          getTableWithUnion'uni req x >>= \case
            Union'UnionA x -> getUnionA'x req x `shouldBe` Just "hi"
            _              -> unexpectedUnionType

          x <- decode $ encode $ tableWithUnion (Just (union (unionB (Just maxBound))))
          getTableWithUnion'uni req x >>= \case
            Union'UnionB x -> getUnionB'y x `shouldBe` Just maxBound
            _              -> unexpectedUnionType

          x <- decode $ encode $ tableWithUnion (Just none)
          getTableWithUnion'uni req x `shouldThrow` \err -> err == MissingField "uni"

        it "optional mode" $ do
          x <- decode $ encode $ tableWithUnion (Just (union (unionA (Just "hi"))))
          getTableWithUnion'uni opt x >>= \case
            Just (Union'UnionA x) -> getUnionA'x req x `shouldBe` Just "hi"
            _                     -> unexpectedUnionType

          x <- decode $ encode $ tableWithUnion (Just (union (unionB (Just maxBound))))
          getTableWithUnion'uni opt x >>= \case
            Just (Union'UnionB x) -> getUnionB'y x `shouldBe` Just maxBound
            _                     -> unexpectedUnionType

          x <- decode $ encode $ tableWithUnion (Just none)
          getTableWithUnion'uni opt x >>= \case
            Nothing -> pure ()
            _       -> unexpectedUnionType

      describe "missing" $ do
        it "required mode" $ do
          x <- decode $ encode $ tableWithUnion Nothing
          getTableWithUnion'uni req x `shouldThrow` \err -> err == MissingField "uni"

        it "optional mode" $ do
          x <- decode $ encode $ tableWithUnion Nothing
          getTableWithUnion'uni opt x >>= \case
            Nothing -> pure ()
            _       -> unexpectedUnionType

      it "throws when union type is present, but union value is missing" $ do
        let union = writeUnion 1 (writeTable [w @Text "hello"])
        x <- decode $ encode $ writeTable @TableWithUnion [wType union]
        getTableWithUnion'uni opt x `shouldThrow` \err -> err == MalformedBuffer "Union: 'union type' found but 'union value' is missing."

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
          Just (Union'UnionA x) -> getUnionA'x req x `shouldBe` Just "hi"
          _                     -> unexpectedUnionType
        xs `index` 1 >>= \case
          Nothing -> pure ()
          _       -> unexpectedUnionType
        xs `index` 2 >>= \case
          Just (Union'UnionB x) -> getUnionB'y x `shouldBe` Just 98
          _                     -> unexpectedUnionType
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


    describe "Align" $ do
      it "present" $ do
        root <- decode $ encode $ alignT
                (Just (align1 11))
                (Just (align2 22 33 44))
                (Just [align1 101, align1 102, align1 103])
                (Just [align2 104 105 106, align2 107 108 109, align2 110 111 112])

        a1 <- getAlignT'x req root
        a2 <- getAlignT'y req root
        a1s <- getAlignT'xs req root >>= toList
        a2s <- getAlignT'ys req root >>= toList

        getAlign1'x a1 `shouldBe` Just 11

        getAlign1'x (getAlign2'x a2) `shouldBe` Just 22
        getAlign2'y a2 `shouldBe` Just 33
        getAlign2'z a2 `shouldBe` Just 44

        traverse getAlign1'x a1s `shouldBe` Just [101, 102, 103]

        forM a2s (\a2 -> (,,) <$> getAlign1'x (getAlign2'x a2) <*> getAlign2'y a2 <*> getAlign2'z a2)
          `shouldBe` Just [(104, 105, 106), (107, 108, 109), (110, 111, 112)]


      it "missing" $ do
        root <- decode $ encode $ alignT Nothing Nothing Nothing Nothing

        getAlignT'x req root `shouldThrow` \err -> err == MissingField "x"
        getAlignT'y req root `shouldThrow` \err -> err == MissingField "y"
        getAlignT'xs req root `shouldThrow` \err -> err == MissingField "xs"
        getAlignT'ys req root `shouldThrow` \err -> err == MissingField "ys"

        getAlignT'x opt root >>= \mb -> isNothing mb `shouldBe` True
        getAlignT'y opt root >>= \mb -> isNothing mb `shouldBe` True
        getAlignT'xs opt root >>= \mb -> isNothing mb `shouldBe` True
        getAlignT'ys opt root >>= \mb -> isNothing mb `shouldBe` True

unexpectedUnionType = expectationFailure "Unexpected union type"
