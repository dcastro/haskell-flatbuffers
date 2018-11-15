{-# LANGUAGE OverloadedStrings #-}

module AlignmentSpec where

import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import           Data.Int
import           Data.Semigroup              (Max (..))
import           Data.WithShow               (WithShow (WS))
import qualified Data.WithShow               as WS
import           FlatBuffers                 as F
import qualified FlatBuffers.Gen             as FG
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as G
import qualified Hedgehog.Range              as R
import           Test.Hspec

spec :: Spec
spec =
  describe "alignment" $ do
    it "int64 are properly aligned" $ require $
      alignedProp
        (WS "scalar int64 maxBound" (scalar int64 maxBound))
        (FG.scalar FG.int64)
        (B.int64LE maxBound)
        8
    it "strings are properly aligned" $ require $
      alignedProp
        (WS "string \"hellohellohello\"" (string "hellohellohello"))
        FG.string
        (B.int32LE 15 <> B.stringUtf8 "hellohellohello")
        4

alignedProp :: WithShow Field -> Gen (WithShow Field) -> B.Builder -> Int64 -> Property
alignedProp field gen expectedBs align =
  property $ do
    bs <-
      fmap (root . table . WS.value) . forAll $
      FG.fieldsWith field gen
    let indices = find (B.toLazyByteString expectedBs) bs
    assert $ any (\i -> i `mod` align == 0) indices

-- | Finds all indices of a substring in a bytestring.
find :: BSL.ByteString -> BSL.ByteString -> [Int64]
find = go 0
  where
    go :: Int64 -> BSL.ByteString -> BSL.ByteString -> [Int64]
    go i sub xs =
      case BSL.uncons xs of
        Just (h, t) ->
          if sub `BSL.isPrefixOf` xs
            then i : go (i + 1) sub t
            else go (i + 1) sub t
        Nothing ->
          if sub `BSL.isPrefixOf` xs
            then [i]
            else []
