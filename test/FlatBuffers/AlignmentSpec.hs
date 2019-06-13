{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TypeApplications  #-}

module FlatBuffers.AlignmentSpec where

import           Control.Monad.State.Strict
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import           Data.Int
import           Data.Semigroup              (Max (..))
import           Data.WithShow               (WithShow (WS))
import qualified Data.WithShow               as WS
import           Data.Word                   (Word8)
import           FlatBuffers.Constants
import qualified FlatBuffers.Gen             as FG
import           FlatBuffers.Internal.Debug
import           FlatBuffers.Internal.Write
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

-- | Appends a flatbuffer to a given pre-built bytestring
testRoot :: B.Builder -> Field -> BSL.ByteString
testRoot initialBuffer table =
  B.toLazyByteString $
  _builder $
  execState
    (do ref <- dump table
        root' ref)
    (FBState initialBuffer initialLength (Max 1) mempty)
  where
    initialLength =
      fromIntegral $ BSL.length (B.toLazyByteString initialBuffer)

root' :: InlineField -> State FBState ()
root' ref = do
  align <- gets (getMax . _maxAlign)
  prep align uoffsetSize
  write ref

newtype PrettyBuffer = PrettyBuffer BSL.ByteString
  deriving Eq

instance Show PrettyBuffer where
  show (PrettyBuffer bs) = showBuffer bs

bufferShouldBe :: BSL.ByteString -> [Word8] -> Expectation
bufferShouldBe bs xs = PrettyBuffer bs `shouldBe` PrettyBuffer (BSL.pack xs)

spec :: Spec
spec =
  describe "alignment" $ do
    describe "structs alignment" $ do
      let initialBuffer = B.word8 99 <> B.word8 99
      it "3 bytes aligned to 1 byte" $ do
        let b = testRoot initialBuffer $ table
                [ inline (struct 1)
                    [word8 33, word8 22, word8 11] ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 10,0,7,0
          , 6,0,0,0
          , 0,0,0,11
          , 22,33,99,99
          ]

      it "3 bytes aligned to 2 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ inline (struct 2)
                    [padded 1 $ word8 33, word8 22, word8 11] ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 10,0,6,0
          , 6,0,0,0
          , 0,0,11,22
          , 33,0,99,99
          ]

      it "3 bytes aligned to 4 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ inline (struct 4)
                    [padded 1 $ word8 33, word8 22, word8 11] ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 10,0,4,0
          , 6,0,0,0
          , 11,22,33,0
          , 0,0,99,99
          ]

      it "3 bytes aligned to 8 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ inline (struct 8)
                    [padded 5 $ word8 33, word8 22, word8 11] ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 18,0,4,0
          , 6,0,0,0
          , 11,22,33,0
          , 0,0,0,0
          , 0,0,0,0
          , 0,0,99,99
          ]

      it "3 bytes aligned to 16 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ inline (struct 16)
                    [padded 13 $ word8 33, word8 22, word8 11] ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 34,0,4,0
          , 6,0,0,0
          , 11,22,33,0
          , 0,0,0,0
          , 0,0,0,0
          , 0,0,0,0
          , 0,0,0,0
          , 0,0,0,0
          , 0,0,0,0
          , 0,0,99,99
          ]

      it "vector of struct with 3 bytes aligned to 1 byte" $ do
        let b = testRoot initialBuffer $ table
                [ vector @[]
                  [ inline (struct 1) [word8 33, word8 22, word8 11]
                  , inline (struct 1) [word8 66, word8 55, word8 44]
                  ]
                ]
        b `bufferShouldBe`
          [ 12, 0, 0, 0
          , 0, 0, 6, 0
          , 8, 0, 4, 0
          , 6, 0, 0, 0
          , 4, 0, 0, 0
          , 2, 0, 0, 0
          , 11, 22, 33, 44
          , 55, 66, 99, 99
          ]

      it "vector of struct with 3 bytes aligned to 2 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ vector @[]
                  [ inline (struct 2) [padded 1 $ word8 33, word8 22, word8 11]
                  , inline (struct 2) [padded 1 $ word8 66, word8 55, word8 44]
                  ]
                ]
        b `bufferShouldBe`
          [ 12, 0, 0, 0
          , 0, 0, 6, 0
          , 8, 0, 4, 0
          , 6, 0, 0, 0
          , 4, 0, 0, 0
          , 2, 0, 0, 0
          , 11, 22, 33, 0
          , 44, 55, 66, 0
          , 0, 0, 99, 99
          ]

      it "vector of struct with 3 bytes aligned to 4 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ vector @[]
                  [ inline (struct 4) [padded 1 $ word8 33, word8 22, word8 11]
                  , inline (struct 4) [padded 1 $ word8 66, word8 55, word8 44]
                  ]
                ]
        b `bufferShouldBe`
          [ 12, 0, 0, 0
          , 0, 0, 6, 0
          , 8, 0, 4, 0
          , 6, 0, 0, 0
          , 4, 0, 0, 0
          , 2, 0, 0, 0
          , 11, 22, 33, 0
          , 44, 55, 66, 0
          , 0, 0, 99, 99
          ]

      it "vector of struct with 3 bytes aligned to 8 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ vector @[]
                  [ inline (struct 8) [padded 5 $ word8 33, word8 22, word8 11]
                  , inline (struct 8) [padded 5 $ word8 66, word8 55, word8 44]
                  ]
                ]
        b `bufferShouldBe`
          [ 12, 0, 0, 0
          , 0, 0, 6, 0
          , 8, 0, 4, 0
          , 6, 0, 0, 0
          , 4, 0, 0, 0
          , 2, 0, 0, 0
          , 11, 22, 33, 0
          , 0, 0, 0, 0
          , 44, 55, 66, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 99, 99
          ]

      it "vector of struct with 3 bytes aligned to 16 bytes" $ do
        let b = testRoot initialBuffer $ table
                [ vector @[]
                  [ inline (struct 16) [padded 13 $ word8 33, word8 22, word8 11]
                  , inline (struct 16) [padded 13 $ word8 66, word8 55, word8 44]
                  ]
                ]
        b `bufferShouldBe`
          [ 20, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 6, 0
          , 8, 0, 4, 0
          , 6, 0, 0, 0
          , 4, 0, 0, 0
          , 2, 0, 0, 0
          , 11, 22, 33, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 44, 55, 66, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 99, 99
          ]

    it "int64 are properly aligned" $ require $
      alignedProp
        (WS "inline int64 maxBound" (inline int64 maxBound))
        (FG.inline FG.int64)
        (B.int64LE maxBound)
        8
    it "strings are properly aligned" $ require $
      alignedProp
        (WS "string \"hellohellohello\"" (text "hellohellohello"))
        FG.text
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
        Just (_h, t) ->
          if sub `BSL.isPrefixOf` xs
            then i : go (i + 1) sub t
            else go (i + 1) sub t
        Nothing ->
          if sub `BSL.isPrefixOf` xs
            then [i]
            else []
