{-# LANGUAGE OverloadedStrings #-}

module AlignmentSpec where

import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import           Data.Semigroup              (Max (..))
import           FlatBuffers                 as F
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as G
import qualified Hedgehog.Range              as R
import           Test.Hspec

spec :: Spec
spec =
  describe "alignment" $
  it "a string's size is aligned to 4 bytes" $
  requireProperty $ do
    bs <- forAll $ G.bytes (R.linear 0 20)
    buffer <- forAll $ G.bytes (R.linear 0 30)
    let state = FBState (B.byteString buffer) (BS.length buffer) (Max 1) mempty
    let finalBs =
          BSL.toStrict $
          B.toLazyByteString $
          _builder $ flip execState state $ dump (byteString bs)
    let padding = BS.length finalBs - (4 + BS.length bs + 1 + BS.length buffer)
    BS.length finalBs `mod` 4 === 0
    assert $ padding >= 0
    assert $ padding < 4

