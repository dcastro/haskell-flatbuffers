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
      x <- decode @Primitives $ root $ encodePrimitives
        (word8 maxBound) (word16 maxBound) (word32 maxBound) (word64 maxBound)
        (int8 maxBound) (int16 maxBound) (int32 maxBound) (int64 maxBound)
        (float 1234.56) (double 2873242.82782) (bool True)
      primitivesA x `shouldBe` Just maxBound
      primitivesB x `shouldBe` Just maxBound
      primitivesC x `shouldBe` Just maxBound
      primitivesD x `shouldBe` Just maxBound
      primitivesE x `shouldBe` Just maxBound
      primitivesF x `shouldBe` Just maxBound
      primitivesG x `shouldBe` Just maxBound
      primitivesH x `shouldBe` Just maxBound
      primitivesI x `shouldBe` Just 1234.56
      primitivesJ x `shouldBe` Just 2873242.82782
      primitivesK x `shouldBe` Just True

      shouldBe 1 1
    describe "Union" $ do
      it "all fields present" $ do
        x <- decode $ root $ encodeUnionByteBool
                (encodeColor Red)
                (encodeUnionUnionA (encodeUnionA (text "hi")))
                (bool True)
        unionByteBoolColor x `shouldBe` Just Red
        unionByteBoolBoo x `shouldBe` Just True
        unionByteBoolUnion x >>= \case
          UnionUnionA x -> unionAX x `shouldBe` Just "hi"
          _             -> expectationFailure "Unexpected union type"

        x <- decode $ root $ encodeUnionByteBool
                missing
                (encodeUnionUnionB (encodeUnionB (int32 maxBound)))
                (bool False)
        unionByteBoolBoo x `shouldBe` Just False
        unionByteBoolUnion x >>= \case
          UnionUnionB x -> unionBX x `shouldBe` Just maxBound
          _             -> expectationFailure "Unexpected union type"

        x <- decode $ root $ encodeUnionByteBool missing encodeUnionNone missing
        unionByteBoolUnion x >>= \case
          UnionNone -> pure ()
          _ -> expectationFailure "Unexpected union type"

      it "all fields missing" $ do
        x <- decode $ root $ encodeUnionByteBool missing (missing, missing) missing
        unionByteBoolColor x `shouldThrow` \x -> x == MissingField "color"
        unionByteBoolUnion x `shouldThrow` \x -> x == MissingField "union"
        unionByteBoolBoo x `shouldThrow` \x -> x == MissingField "boo"

----------------------------------
---------- Primitives ------------
----------------------------------

newtype Primitives =
  Primitives Table
  deriving (HasPosition)

encodePrimitives ::
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
encodePrimitives a b c d e f g h i j k =
  Tagged $ F.table [untag a, untag b, untag c, untag d, untag e, untag f, untag g, untag h, untag i, untag j, untag k]

primitivesA :: ReadCtx m => Primitives -> m Word8
primitivesB :: ReadCtx m => Primitives -> m Word16
primitivesC :: ReadCtx m => Primitives -> m Word32
primitivesD :: ReadCtx m => Primitives -> m Word64
primitivesE :: ReadCtx m => Primitives -> m Int8
primitivesF :: ReadCtx m => Primitives -> m Int16
primitivesG :: ReadCtx m => Primitives -> m Int32
primitivesH :: ReadCtx m => Primitives -> m Int64
primitivesI :: ReadCtx m => Primitives -> m Float
primitivesJ :: ReadCtx m => Primitives -> m Double
primitivesK :: ReadCtx m => Primitives -> m Bool
primitivesA x = tableIndexToVOffset x 0 >>= required "a" (readPrim . move x)
primitivesB x = tableIndexToVOffset x 1 >>= required "b" (readPrim . move x)
primitivesC x = tableIndexToVOffset x 2 >>= required "c" (readPrim . move x)
primitivesD x = tableIndexToVOffset x 3 >>= required "d" (readPrim . move x)
primitivesE x = tableIndexToVOffset x 4 >>= required "e" (readPrim . move x)
primitivesF x = tableIndexToVOffset x 5 >>= required "f" (readPrim . move x)
primitivesG x = tableIndexToVOffset x 6 >>= required "g" (readPrim . move x)
primitivesH x = tableIndexToVOffset x 7 >>= required "h" (readPrim . move x)
primitivesI x = tableIndexToVOffset x 8 >>= required "i" (readPrim . move x)
primitivesJ x = tableIndexToVOffset x 9 >>= required "j" (readPrim . move x)
primitivesK x = tableIndexToVOffset x 10 >>= required "k" (readPrim . move x)

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

encodeColor :: Color -> Tagged Color Field
encodeColor c =
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

encodeUnionA :: Tagged Text Field -> Tagged UnionA Field
encodeUnionA x1 = Tagged $ F.table [untag x1]

unionAX :: ReadCtx m => UnionA -> m Text
unionAX x = tableIndexToVOffset x 0 >>= required "x" (readText . move x)

----------------------------------
------------- UnionB -------------
----------------------------------
newtype UnionB =
  UnionB Table
  deriving (HasPosition)

encodeUnionB :: Tagged Int32 Field -> Tagged UnionB Field
encodeUnionB x1 = Tagged $ F.table [untag x1]

unionBX :: ReadCtx m => UnionB -> m Int32
unionBX x = tableIndexToVOffset x 0 >>= required "x" (readPrim . move x)

----------------------------------
------------- Union --------------
----------------------------------
data Union
  = UnionNone
  | UnionUnionA !UnionA
  | UnionUnionB !UnionB

encodeUnionNone :: (Tagged Word8 Field, Tagged Union Field)
encodeUnionNone = (word8 0, missing)

encodeUnionUnionA :: Tagged UnionA Field -> (Tagged Word8 Field, Tagged Union Field)
encodeUnionUnionA x = (word8 1, coerce x)

encodeUnionUnionB :: Tagged UnionB Field -> (Tagged Word8 Field, Tagged Union Field)
encodeUnionUnionB x = (word8 2, coerce x)

readUnion :: ReadCtx m => Word8 -> Position -> m Union
readUnion n pos =
  case n of
    0 -> pure UnionNone
    1 -> fmap (UnionUnionA . UnionA) (readTable pos)
    2 -> fmap (UnionUnionB . UnionB) (readTable pos)
    _ -> throwM $ UnionUnknown "Union" n

----------------------------------
------- UnionByteBool ------------
----------------------------------
newtype UnionByteBool =
  UnionByteBool Table
  deriving (HasPosition)

encodeUnionByteBool ::
     Tagged Color Field
  -> (Tagged Word8 Field, Tagged Union Field)
  -> Tagged Bool Field
  -> Tagged UnionByteBool Field
encodeUnionByteBool x1 x2 x3 =
  Tagged $ F.table [untag x1, untag (fst x2), untag (snd x2), untag x3]

unionByteBoolColor :: ReadCtx m => UnionByteBool -> m Color
unionByteBoolColor x = tableIndexToVOffset x 0 >>= required "color" (readColor . move x)

unionByteBoolUnion :: ReadCtx m => UnionByteBool -> m Union
unionByteBoolUnion x = do
  n <- tableIndexToVOffset x 1 >>= required "union" (readPrim . move x)
  if n == 0
    then pure UnionNone
    else tableIndexToVOffset x 2 >>= required "union" (readUnion n . move x)

unionByteBoolBoo :: ReadCtx m => UnionByteBool -> m Bool
unionByteBoolBoo x = tableIndexToVOffset x 3 >>= required "boo" (readPrim . move x)

