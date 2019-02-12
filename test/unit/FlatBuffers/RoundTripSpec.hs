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
import qualified FlatBuffers            as F
import           FlatBuffers.Dsl
import           FlatBuffers.Read
import           Test.Hspec

unexpectedUnionType = expectationFailure "Unexpected union type"

spec :: Spec
spec =
  describe "Round Trip" $ do
    it "Primitives" $ do
      x <- decode @Primitives $ root $ primitives
        (word8 maxBound) (word16 maxBound) (word32 maxBound) (word64 maxBound)
        (int8 maxBound) (int16 maxBound) (int32 maxBound) (int64 maxBound)
        (float 1234.56) (double 2873242.82782) (bool True)
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

    describe "Enums" $ do
      it "present" $ do
        x <- decode @Enums $ root $ enums (color Red)
        getEnums'x x `shouldBe` Just Red
      it "missing" $ do
        x <- decode @Enums $ root $ enums missing
        getEnums'x x `shouldThrow` \x -> x == MissingField "x"

    describe "Union" $ do
      it "present" $ do
        x <- decode $ root $ tableWithUnion (union'unionA (unionA (text "hi")))
        getTableWithUnion'uni x >>= \case
          Union'UnionA x -> getUnionA'x x `shouldBe` Just "hi"
          _              -> unexpectedUnionType

        x <- decode $ root $ tableWithUnion (union'unionB (unionB (int32 maxBound)))
        getTableWithUnion'uni x >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just maxBound
          _              -> unexpectedUnionType

        x <- decode $ root $ tableWithUnion union'none
        getTableWithUnion'uni x >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType

      it "missing" $ do
        x <- decode $ root $ tableWithUnion (missing, missing)
        getTableWithUnion'uni x `shouldThrow` \x -> x == MissingField "uni"

    describe "VectorOfUnions" $ do
      it "present" $ do
        x <- decode $ root $ vectorOfUnions (unionVector
          [ union'unionA (unionA (text "hi"))
          , union'unionB (unionB (int32 98))
          ])
        xs <- getVectorOfUnions'xs x
        readElem 0 xs >>= \case
          Union'UnionA x -> getUnionA'x x `shouldBe` Just "hi"
          _              -> unexpectedUnionType
        readElem 1 xs >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just 98
          _              -> unexpectedUnionType
        readElem 2 xs `shouldThrow` \err -> err == VectorIndexOutOfBounds 2 2

      it "missing" $ do
        x <- decode $ root $ vectorOfUnions (missing, missing)
        getVectorOfUnions'xs x `shouldThrow` \err -> err == MissingField "xs"

----------------------------------
---------- Primitives ------------
----------------------------------
newtype Primitives =
  Primitives Table
  deriving (HasPosition)

primitives ::
     Tagged Word8 Field
  -> Tagged Word16 Field
  -> Tagged Word32 Field
  -> Tagged Word64 Field
  -> Tagged Int8 Field
  -> Tagged Int16 Field
  -> Tagged Int32 Field
  -> Tagged Int64 Field
  -> Tagged Float Field
  -> Tagged Double Field
  -> Tagged Bool Field
  -> Tagged Primitives Field
primitives a b c d e f g h i j k =
  Tagged $ F.table [untag a, untag b, untag c, untag d, untag e, untag f, untag g, untag h, untag i, untag j, untag k]

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

color :: Color -> Tagged Color Field
color c =
  coerce . word8 $
  case c of
    Red   -> 0
    Green -> 1
    Blue  -> 2
    Gray  -> 5
    Black -> 8

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

enums :: Tagged Color Field -> Tagged Enums Field
enums x1 = Tagged $ F.table [untag x1]

getEnums'x :: ReadCtx m => Enums -> m Color
getEnums'x x = tableIndexToVOffset x 0 >>= required "x" (readColor . move x)

----------------------------------
------------- UnionA -------------
----------------------------------
newtype UnionA =
  UnionA Table
  deriving (HasPosition)

unionA :: Tagged Text Field -> Tagged UnionA Field
unionA x1 = Tagged $ F.table [untag x1]

getUnionA'x :: ReadCtx m => UnionA -> m Text
getUnionA'x x = tableIndexToVOffset x 0 >>= required "x" (readText . move x)

----------------------------------
------------- UnionB -------------
----------------------------------
newtype UnionB =
  UnionB Table
  deriving (HasPosition)

unionB :: Tagged Int32 Field -> Tagged UnionB Field
unionB x1 = Tagged $ F.table [untag x1]

getUnionB'y :: ReadCtx m => UnionB -> m Int32
getUnionB'y x = tableIndexToVOffset x 0 >>= required "y" (readPrim . move x)

----------------------------------
------------- Union --------------
----------------------------------
data Union
  = Union'None
  | Union'UnionA !UnionA
  | Union'UnionB !UnionB

union'none :: (Tagged Word8 Field, Tagged Union Field)
union'none = (word8 0, missing)

union'unionA :: Tagged UnionA Field -> (Tagged Word8 Field, Tagged Union Field)
union'unionA x = (word8 1, coerce x)

union'unionB :: Tagged UnionB Field -> (Tagged Word8 Field, Tagged Union Field)
union'unionB x = (word8 2, coerce x)

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

tableWithUnion :: (Tagged Word8 Field, Tagged Union Field) -> Tagged TableWithUnion Field
tableWithUnion x1 =
  Tagged $ F.table [untag (fst x1), untag (snd x1)]


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

vectorOfUnions :: (Tagged [Word8] Field, Tagged [Union] Field) -> Tagged VectorOfUnions Field
vectorOfUnions x1 =
  Tagged $ F.table [untag (fst x1), untag (snd x1)]

getVectorOfUnions'xs :: ReadCtx m => VectorOfUnions -> m (Vector Union)
getVectorOfUnions'xs x =
  do
    i <- tableIndexToVOffset x 0 >>= required "xs" (pure . move x)
    j <- tableIndexToVOffset x 1 >>= required "xs" (pure . move x)
    readUnionVector Union'None readUnion i j
