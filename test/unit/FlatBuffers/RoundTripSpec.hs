{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module FlatBuffers.RoundTripSpec where

import           Control.Exception.Safe (throwM)
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Int
import           Data.Tagged            (Tagged (..), untag)
import           Data.Text
import           Data.Word
import           FlatBuffers.Read
import           FlatBuffers.Write
import           Test.Hspec

unexpectedUnionType = expectationFailure "Unexpected union type"

spec :: Spec
spec =
  describe "Round Trip" $ do
    describe "Primitives" $ do
      it "present" $ do
        x <- decode @Primitives $ encode $ primitives
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
        x <- decode @Primitives $ encode $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing
        getPrimitives'a x `shouldThrow` \x -> x == MissingField "a"
        getPrimitives'b x `shouldThrow` \x -> x == MissingField "b"
        getPrimitives'c x `shouldThrow` \x -> x == MissingField "c"
        getPrimitives'd x `shouldThrow` \x -> x == MissingField "d"
        getPrimitives'e x `shouldThrow` \x -> x == MissingField "e"
        getPrimitives'f x `shouldThrow` \x -> x == MissingField "f"
        getPrimitives'g x `shouldThrow` \x -> x == MissingField "g"
        getPrimitives'h x `shouldThrow` \x -> x == MissingField "h"
        getPrimitives'i x `shouldThrow` \x -> x == MissingField "i"
        getPrimitives'j x `shouldThrow` \x -> x == MissingField "j"
        getPrimitives'k x `shouldThrow` \x -> x == MissingField "k"

    describe "Enums" $ do
      it "present" $ do
        x <- decode $ encode $ enums (Just Red)
        getEnums'x x `shouldBe` Just Red
      it "missing" $ do
        x <- decode @Enums $ encode $ enums Nothing
        getEnums'x x `shouldThrow` \x -> x == MissingField "x"

    describe "Union" $ do
      it "present" $ do
        x <- decode $ encode $ tableWithUnion (Just (union'unionA (unionA (Just "hi"))))
        getTableWithUnion'uni x >>= \case
          Union'UnionA x -> getUnionA'x x `shouldBe` Just "hi"
          _              -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just (union'unionB (unionB (Just maxBound))))
        getTableWithUnion'uni x >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just maxBound
          _              -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just none)
        getTableWithUnion'uni x >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType

      it "missing" $ do
        x <- decode $ encode $ tableWithUnion Nothing
        getTableWithUnion'uni x `shouldThrow` \x -> x == MissingField "uni"

    describe "VectorOfUnions" $ do
      it "present" $ do
        x <- decode $ encode $ vectorOfUnions (Just
          [ union'unionA (unionA (Just "hi"))
          , none
          , union'unionB (unionB (Just 98))
          ])
        xs <- getVectorOfUnions'xs x
        vectorLength xs `shouldBe` 3
        readElem 0 xs >>= \case
          Union'UnionA x -> getUnionA'x x `shouldBe` Just "hi"
          _              -> unexpectedUnionType
        readElem 1 xs >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType
        readElem 2 xs >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just 98
          _              -> unexpectedUnionType
        readElem 3 xs `shouldThrow` \err -> err == VectorIndexOutOfBounds 3 3

      it "missing" $ do
        x <- decode $ encode $ vectorOfUnions Nothing
        getVectorOfUnions'xs x `shouldThrow` \err -> err == MissingField "xs"
        
----------------------------------
---------- Primitives ------------
----------------------------------
newtype Primitives =
  Primitives Table
  deriving (HasPosition)

primitives ::
     Maybe Word8
  -> Maybe Word16
  -> Maybe Word32
  -> Maybe Word64
  -> Maybe Int8
  -> Maybe Int16
  -> Maybe Int32
  -> Maybe Int64
  -> Maybe Float
  -> Maybe Double
  -> Maybe Bool
  -> Tagged Primitives Field
primitives a b c d e f g h i j k =
  Tagged $ table [w a, w b, w c, w d, w e, w f, w g, w h, w i, w j, w k]

getPrimitives'a :: ReadCtx m => Primitives -> m Word8
getPrimitives'b :: ReadCtx m => Primitives -> m Word16
getPrimitives'c :: ReadCtx m => Primitives -> m Word32
getPrimitives'd :: ReadCtx m => Primitives -> m Word64
getPrimitives'e :: ReadCtx m => Primitives -> m Int8
getPrimitives'f :: ReadCtx m => Primitives -> m Int16
getPrimitives'g :: ReadCtx m => Primitives -> m Int32
getPrimitives'h :: ReadCtx m => Primitives -> m Int64
getPrimitives'i :: ReadCtx m => Primitives -> m Float
getPrimitives'j :: ReadCtx m => Primitives -> m Double
getPrimitives'k :: ReadCtx m => Primitives -> m Bool
getPrimitives'a x = tableIndexToVOffset x 0 >>= required "a" (readPrim . move x)
getPrimitives'b x = tableIndexToVOffset x 1 >>= required "b" (readPrim . move x)
getPrimitives'c x = tableIndexToVOffset x 2 >>= required "c" (readPrim . move x)
getPrimitives'd x = tableIndexToVOffset x 3 >>= required "d" (readPrim . move x)
getPrimitives'e x = tableIndexToVOffset x 4 >>= required "e" (readPrim . move x)
getPrimitives'f x = tableIndexToVOffset x 5 >>= required "f" (readPrim . move x)
getPrimitives'g x = tableIndexToVOffset x 6 >>= required "g" (readPrim . move x)
getPrimitives'h x = tableIndexToVOffset x 7 >>= required "h" (readPrim . move x)
getPrimitives'i x = tableIndexToVOffset x 8 >>= required "i" (readPrim . move x)
getPrimitives'j x = tableIndexToVOffset x 9 >>= required "j" (readPrim . move x)
getPrimitives'k x = tableIndexToVOffset x 10 >>= required "k" (readPrim . move x)

----------------------------------
------------- Color --------------
----------------------------------
data Color
  = Red
  | Green
  | Blue
  | Gray
  | Black
  deriving (Eq, Show, Enum)

instance AsTableField Color where
  w = scalar ws
instance AsStructField Color where
  ws x =
    ws $
    case x of
      Red   -> 0 :: Word8
      Green -> 1 :: Word8
      Blue  -> 2 :: Word8
      Gray  -> 5 :: Word8
      Black -> 8 :: Word8

readColor :: ReadCtx m => Position -> m Color
readColor p =
  readPrim p >>= \n ->
    case (n :: Word8) of
      0 -> pure Red
      1 -> pure Green
      2 -> pure Blue
      5 -> pure Gray
      8 -> pure Black
      _ -> throwM $ EnumUnknown "Color" (fromIntegral @Word8 @Word64 n)


----------------------------------
------------- Enums --------------
----------------------------------
newtype Enums =
  Enums Table
  deriving (HasPosition)

enums :: Maybe Color -> Tagged Enums Field
enums x1 = Tagged $ table [w x1]

getEnums'x :: ReadCtx m => Enums -> m Color
getEnums'x x = tableIndexToVOffset x 0 >>= required "x" (readColor . move x)

----------------------------------
------------- UnionA -------------
----------------------------------
newtype UnionA =
  UnionA Table
  deriving (HasPosition)

unionA :: Maybe Text -> Tagged UnionA Field
unionA x1 = Tagged $ table [w x1]

getUnionA'x :: ReadCtx m => UnionA -> m Text
getUnionA'x x = tableIndexToVOffset x 0 >>= required "x" (readText . move x)

----------------------------------
------------- UnionB -------------
----------------------------------
newtype UnionB =
  UnionB Table
  deriving (HasPosition)

unionB :: Maybe Int32 -> Tagged UnionB Field
unionB x1 = Tagged $ table [w x1]

getUnionB'y :: ReadCtx m => UnionB -> m Int32
getUnionB'y x = tableIndexToVOffset x 0 >>= required "y" (readPrim . move x)

----------------------------------
------------- Union --------------
----------------------------------
data Union
  = Union'None
  | Union'UnionA !UnionA
  | Union'UnionB !UnionB

union'unionA :: Tagged UnionA Field -> Tagged Union UnionField
union'unionA x = Tagged (Some (1, untag x))

union'unionB :: Tagged UnionB Field -> Tagged Union UnionField
union'unionB x = Tagged (Some (2, untag x))

readUnion :: ReadCtx m => Word8 -> Position -> m Union
readUnion n pos =
  case n of
    0 -> pure Union'None
    1 -> fmap (Union'UnionA . UnionA) (readTable pos)
    2 -> fmap (Union'UnionB . UnionB) (readTable pos)
    _ -> throwM $ UnionUnknown "Union" n

----------------------------------
------- TableWithUnion -----------
----------------------------------
newtype TableWithUnion =
  TableWithUnion Table
  deriving (HasPosition)

tableWithUnion :: Maybe (Tagged Union UnionField) -> Tagged TableWithUnion Field
tableWithUnion x1 =
  Tagged $ table [wType x1, wValue x1]


getTableWithUnion'uni :: ReadCtx m => TableWithUnion -> m Union
getTableWithUnion'uni x = do
  n <- tableIndexToVOffset x 0 >>= required "uni" (readPrim . move x)
  if n == 0
    then pure Union'None
    else tableIndexToVOffset x 1 >>= required "uni" (readUnion n . move x)

----------------------------------
------- VectorOfUnions -----------
----------------------------------
newtype VectorOfUnions =
  VectorOfUnions Table
  deriving (HasPosition)

vectorOfUnions :: Maybe [Tagged Union UnionField] -> Tagged VectorOfUnions Field
vectorOfUnions x1 =
  Tagged $ table [wType x1, wValue x1]

getVectorOfUnions'xs :: ReadCtx m => VectorOfUnions -> m (Vector Union)
getVectorOfUnions'xs x =
  do
    i <- tableIndexToVOffset x 0 >>= required "xs" (pure . move x)
    j <- tableIndexToVOffset x 1 >>= required "xs" (pure . move x)
    readUnionVector Union'None readUnion i j
