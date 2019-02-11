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

      shouldBe 1 1
    describe "Union" $ do
      it "all fields present" $ do
        x <- decode $ root $ unionByteBool
                (color Red)
                (union'unionA (unionA (text "hi")))
                (bool True)
        getUnionByteBool'color x `shouldBe` Just Red
        getUnionByteBool'boo x `shouldBe` Just True
        getUnionByteBool'uni x >>= \case
          Union'UnionA x -> unionA'x x `shouldBe` Just "hi"
          _             -> expectationFailure "Unexpected union type"

        x <- decode $ root $ unionByteBool
                missing
                (union'unionB (unionB (int32 maxBound)))
                (bool False)
        getUnionByteBool'boo x `shouldBe` Just False
        getUnionByteBool'uni x >>= \case
          Union'UnionB x -> getUnionB'x x `shouldBe` Just maxBound
          _             -> expectationFailure "Unexpected union type"

        x <- decode $ root $ unionByteBool missing union'none missing
        getUnionByteBool'uni x >>= \case
          Union'None -> pure ()
          _ -> expectationFailure "Unexpected union type"

      it "all fields missing" $ do
        x <- decode $ root $ unionByteBool missing (missing, missing) missing
        getUnionByteBool'color x `shouldThrow` \x -> x == MissingField "color"
        getUnionByteBool'uni x `shouldThrow` \x -> x == MissingField "union"
        getUnionByteBool'boo x `shouldThrow` \x -> x == MissingField "boo"

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
------------- UnionA -------------
----------------------------------
newtype UnionA =
  UnionA Table
  deriving (HasPosition)

unionA :: Tagged Text Field -> Tagged UnionA Field
unionA x1 = Tagged $ F.table [untag x1]

unionA'x :: ReadCtx m => UnionA -> m Text
unionA'x x = tableIndexToVOffset x 0 >>= required "x" (readText . move x)

----------------------------------
------------- UnionB -------------
----------------------------------
newtype UnionB =
  UnionB Table
  deriving (HasPosition)

unionB :: Tagged Int32 Field -> Tagged UnionB Field
unionB x1 = Tagged $ F.table [untag x1]

getUnionB'x :: ReadCtx m => UnionB -> m Int32
getUnionB'x x = tableIndexToVOffset x 0 >>= required "x" (readPrim . move x)

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
------- UnionByteBool ------------
----------------------------------
newtype UnionByteBool =
  UnionByteBool Table
  deriving (HasPosition)

unionByteBool ::
     Tagged Color Field
  -> (Tagged Word8 Field, Tagged Union Field)
  -> Tagged Bool Field
  -> Tagged UnionByteBool Field
unionByteBool x1 x2 x3 =
  Tagged $ F.table [untag x1, untag (fst x2), untag (snd x2), untag x3]

getUnionByteBool'color :: ReadCtx m => UnionByteBool -> m Color
getUnionByteBool'color x = tableIndexToVOffset x 0 >>= required "color" (readColor . move x)

getUnionByteBool'uni :: ReadCtx m => UnionByteBool -> m Union
getUnionByteBool'uni x = do
  n <- tableIndexToVOffset x 1 >>= required "union" (readPrim . move x)
  if n == 0
    then pure Union'None
    else tableIndexToVOffset x 2 >>= required "union" (readUnion n . move x)

getUnionByteBool'boo :: ReadCtx m => UnionByteBool -> m Bool
getUnionByteBool'boo x = tableIndexToVOffset x 3 >>= required "boo" (readPrim . move x)

