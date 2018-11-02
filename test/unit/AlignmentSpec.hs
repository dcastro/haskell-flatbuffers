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
    it "a string's size is aligned to 4 bytes" $
      requireProperty $ do
        bs <- forAll $ G.bytes (R.linear 0 20)
        buffer <- forAll $ G.bytes (R.linear 0 30)
        let state =
              FBState (B.byteString buffer) (BS.length buffer) (Max 1) mempty
        let finalBs =
              BSL.toStrict $
              B.toLazyByteString $
              _builder $ flip execState state $ dump (byteString bs)
        let padding =
              BS.length finalBs - (4 + BS.length bs + 1 + BS.length buffer)
        BS.length finalBs `mod` 4 === 0
        assert $ padding >= 0
        assert $ padding < 4
    it "int64 are properly aligned" $ require alignedProp

alignedProp :: Property
alignedProp =
  property $ do
    let field = B.int64LE maxBound
    bs <-
      fmap (B.toLazyByteString . root . WS.value) . forAll $
      FG.tableWith (WS "scalar int64 maxBound" (scalar int64 maxBound))
    let indices = find (B.toLazyByteString $ B.int64LE maxBound) bs
    assert $ any (\i -> i `mod` 8 == 0) indices

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
