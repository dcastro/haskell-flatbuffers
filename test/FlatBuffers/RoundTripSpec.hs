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
        x <- decode @Primitives $ encodeWithFileIdentifier $ primitives
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just 1234.56) (Just 2873242.82782) (Just True) (Just "hi 👬 bye")
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
        getPrimitives'l x `shouldBe` Just (Just "hi 👬 bye")

      it "missing" $ do
        x <- decode @Primitives $ encodeWithFileIdentifier $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
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
        getPrimitives'l x `shouldBe` Just Nothing

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
        x <- decode $ encode $ writeTable @TableWithUnion [inline word8 1]
        getTableWithUnion'uni x `shouldThrow` \err -> err == MalformedBuffer "Union: 'union type' found but 'union value' is missing."

    describe "Vectors" $ do
      let
        testPrimVector :: (VectorElement a, Show a, Eq a) => [a] -> Vector a -> IO ()
        testPrimVector list vec = do
          vectorLength vec `shouldBe` Just (L.genericLength list)
          toList vec `shouldBe` Just list
          when (not $ null list) $
            traverse (\i -> vec `index` i) [0 .. L.genericLength list - 1] `shouldBe` Just list

      let Just nonEmptyVecs = decode $ encode $ vectors
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

      let Just emptyVecs = decode $ encode $ vectors
            (Just []) (Just []) (Just []) (Just [])
            (Just []) (Just []) (Just []) (Just [])
            (Just []) (Just []) (Just []) (Just [])

      let Just missingVecs = decode $ encode $ vectors
            Nothing Nothing Nothing Nothing
            Nothing Nothing Nothing Nothing
            Nothing Nothing Nothing Nothing


      describe "word8 vector"  $ do
        it "non empty" $ getVectors'a nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'a emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'a missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "word16 vector" $ do
        it "non empty" $ getVectors'b nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'b emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'b missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "word32 vector" $ do
        it "non empty" $ getVectors'c nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'c emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'c missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "word64 vector" $ do
        it "non empty" $ getVectors'd nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'd emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'd missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "int8 vector"   $ do
        it "non empty" $ getVectors'e nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'e emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'e missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "int16 vector"  $ do
        it "non empty" $ getVectors'f nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'f emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'f missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "int32 vector"  $ do
        it "non empty" $ getVectors'g nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'g emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'g missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "int64 vector"  $ do
        it "non empty" $ getVectors'h nonEmptyVecs >>= testPrimVector [minBound, 0, maxBound] . fromJust
        it "empty"     $ getVectors'h emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'h missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "float vector"  $ do
        it "non empty" $ getVectors'i nonEmptyVecs >>= testPrimVector [-12e9, 0, 3.33333333333333333333] . fromJust
        it "empty"     $ getVectors'i emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'i missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "double vector" $ do
        it "non empty" $ getVectors'j nonEmptyVecs >>= testPrimVector [-12e98, 0, 3.33333333333333333333] . fromJust
        it "empty"     $ getVectors'j emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'j missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "bool vector"   $ do
        it "non empty" $ getVectors'k nonEmptyVecs >>= testPrimVector [True, False, True] . fromJust
        it "empty"     $ getVectors'k emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'k missingVecs >>= traverse toList) `shouldBe` Just Nothing
      describe "string vector" $ do
        it "non empty" $ getVectors'l nonEmptyVecs >>= testPrimVector ["hi 👬 bye", "", "world"] . fromJust
        it "empty"     $ getVectors'l emptyVecs    >>= testPrimVector [] . fromJust
        it "missing"   $ (getVectors'l missingVecs >>= traverse toList) `shouldBe` Just Nothing

    describe "VectorOfUnions" $ do
      it "non empty" $ do
        let
          shouldBeSword swordX (Union (Weapon'Sword s)) = getSword'x s `shouldBe` Just (Just swordX)
          shouldBeSword _ _                             = unexpectedUnionType

          shouldBeAxe axeY (Union (Weapon'Axe s)) = getAxe'y s `shouldBe` Just axeY
          shouldBeAxe _ _                         = unexpectedUnionType

          shouldBeNone UnionNone = pure ()
          shouldBeNone _         = unexpectedUnionType

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
        vectorLength xs `shouldBe` Just 3
        length <$> toList xs `shouldBe` Just 3
        xs `index` 0 >>= shouldBeSword "hi"
        xs `index` 1 >>= shouldBeNone
        xs `index` 2 >>= shouldBeAxe 98
        toList xs <&> (!! 0) >>= shouldBeSword "hi"
        toList xs <&> (!! 1) >>= shouldBeNone
        toList xs <&> (!! 2) >>= shouldBeAxe 98

        xsReq <- getVectorOfUnions'xsReq x
        vectorLength xsReq `shouldBe` Just 3
        length <$> toList xsReq `shouldBe` Just 3
        xsReq `index` 0 >>= shouldBeSword "hi2"
        xsReq `index` 1 >>= shouldBeNone
        xsReq `index` 2 >>= shouldBeAxe 100
        toList xsReq <&> (!! 0) >>= shouldBeSword "hi2"
        toList xsReq <&> (!! 1) >>= shouldBeNone
        toList xsReq <&> (!! 2) >>= shouldBeAxe 100

      it "empty" $ do
        x <- decode $ encode $ vectorOfUnions (Just []) [ ]

        Just xs <- getVectorOfUnions'xs x
        vectorLength xs `shouldBe` Just 0
        length <$> toList xs `shouldBe` Just 0

        xsReq <- getVectorOfUnions'xsReq x
        vectorLength xsReq `shouldBe` Just 0
        length <$> toList xsReq `shouldBe` Just 0
        
      it "missing" $ do
        x <- decode $ encode $ vectorOfUnions Nothing []
        getVectorOfUnions'xs x >>= \mb -> isNothing mb `shouldBe` True
        (getVectorOfUnions'xsReq x >>= vectorLength) `shouldBe` Just 0

      it "throws when union type vector is present, but union value vector is missing" $ do
        x <- decode $ encode $ writeTable @VectorOfUnions
          [ (writeVector . inline) word8 []
          , W.missing
          , (writeVector . inline) word8 []
          , W.missing
          ]
        getVectorOfUnions'xs x `shouldThrow` \err -> err == MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        getVectorOfUnions'xsReq x `shouldThrow` \err -> err == MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."

    describe "VectorOfStructs" $ do
      let getBytes = (liftA3 . liftA3) (,,) getThreeBytes'a getThreeBytes'b getThreeBytes'c
      it "non empty" $ do
        x <- decode $ encode $ vectorOfStructs (Just
          [ threeBytes 1 2 3
          , threeBytes 4 5 6
          ])

        Just xs <- getVectorOfStructs'xs x
        vectorLength xs `shouldBe` Just 2
        (toList xs >>= traverse getBytes) `shouldBe` Just [(1,2,3), (4,5,6)]
        (traverse (index xs) [0..1] >>= traverse getBytes) `shouldBe` Just [(1,2,3), (4,5,6)]

      it "empty" $ do
        x <- decode $ encode $ vectorOfStructs (Just [])

        Just xs <- getVectorOfStructs'xs x
        vectorLength xs `shouldBe` Just 0
        (toList xs >>= traverse getBytes) `shouldBe` Just []

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
