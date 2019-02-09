{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module FlatBuffers.RoundTripSpec where

import           Control.Exception.Safe (throwM)
import           Data.Coerce
import           Data.Tagged            (Tagged (..), untag)
import           Data.Word
import qualified FlatBuffers            as F
import           FlatBuffers.Dsl
import           FlatBuffers.Read
import           Test.Hspec

spec :: Spec
spec =
  describe "Round Trip" $
    describe "Union" $ do
      it "all fields present" $  do
        let bs = root $ encodeUnionByteBool (encodeColor Red)
        x <- decode @UnionByteBool bs

        unionByteBoolColor x `shouldBe` Just Red

        shouldBe 1 1
      it "all fields missing" $ do
        let bs = encodeUnionByteBool missing
        shouldBe 1 1

data Color = Red | Green | Blue | Gray | Black
  deriving (Eq, Show, Enum)

encodeColor :: Color -> Tagged Color Field
encodeColor c = coerce . word8 $ case c of
  Red -> 0
  Green -> 1
  Blue -> 2
  Gray -> 5
  Black -> 8

readColor :: ReadCtx m => Position -> m Color
readColor p = readNumerical p >>= \n -> case (n :: Word8) of
  0 -> pure Red
  1 -> pure Green
  2 -> pure Blue
  5 -> pure Gray
  8 -> pure Black
  _ -> throwM $ EnumUnknownValue "Color" (fromIntegral n)

newtype UnionByteBool =
  UnionByteBool Table
  deriving (HasPosition)

encodeUnionByteBool :: Tagged Color Field -> Tagged UnionByteBool Field
encodeUnionByteBool x1 = Tagged $ F.table [untag x1]

unionByteBoolColor :: ReadCtx m => UnionByteBool -> m Color
unionByteBoolColor x = tableIndexToVOffset x 0 >>= required "color" (readColor . move x)
