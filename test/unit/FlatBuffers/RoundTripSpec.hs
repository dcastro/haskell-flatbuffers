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
  describe "Round Trip" $
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
  readNumerical p >>= \n ->
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
unionBX x = tableIndexToVOffset x 0 >>= required "x" (readNumerical . move x)

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
  n <- tableIndexToVOffset x 1 >>= required "union" (readNumerical . move x)
  if n == 0
    then pure UnionNone
    else tableIndexToVOffset x 2 >>= required "union" (readUnion n . move x)

unionByteBoolBoo :: ReadCtx m => UnionByteBool -> m Bool
unionByteBoolBoo x = tableIndexToVOffset x 3 >>= required "boo" (readNumerical . move x)

