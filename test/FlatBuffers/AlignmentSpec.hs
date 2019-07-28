module FlatBuffers.AlignmentSpec where

import           Control.Monad.State.Strict

import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import           Data.Semigroup              ( getMax )
import           Data.Word                   ( Word8 )

import           FlatBuffers.Constants
import           FlatBuffers.Internal.Build
import           FlatBuffers.Internal.Debug
import           FlatBuffers.Internal.Write
import           FlatBuffers.Types           ( Alignment(..), InlineSize(..) )

import           Test.Hspec

-- | Appends a flatbuffer to a given pre-built bytestring
testEncode :: B.Builder -> WriteTable a -> BSL.ByteString
testEncode initialBuffer (WriteTable table) =
  B.toLazyByteString $
  builder $
  execState
    (do loc <- table
        maxAlignment <- gets (getMax . maxAlign)
        modify' $ alignTo maxAlignment uoffsetSize
        modify' $ uoffsetFrom loc
    )
    (FBState initialBuffer initialLength 1 mempty)
  where
    initialLength =
      fromIntegral $ BSL.length (B.toLazyByteString initialBuffer)

newtype PrettyBuffer = PrettyBuffer BSL.ByteString
  deriving Eq

instance Show PrettyBuffer where
  show (PrettyBuffer bs) = showBuffer bs

bufferShouldBe :: HasCallStack => BSL.ByteString -> [Word8] -> Expectation
bufferShouldBe bs xs = PrettyBuffer bs `shouldBe` PrettyBuffer (BSL.pack xs)

spec :: Spec
spec =
  describe "alignment" $
    describe "structs alignment" $ do
      let initialBuffer = B.word8 99 <> B.word8 99
      it "3 bytes aligned to 1 byte" $ do
        let b = testEncode initialBuffer $ writeTable
                [ writeStructTableField' 1 3 $
                    buildWord8 11 <> buildWord8 22 <> buildWord8 33
                ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 10,0,7,0
          , 6,0,0,0
          , 0,0,0,11
          , 22,33,99,99
          ]

      it "3 bytes aligned to 2 bytes" $ do
        let b = testEncode initialBuffer $ writeTable
                [ writeStructTableField' 2 4 $
                    buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 1 ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 10,0,6,0
          , 6,0,0,0
          , 0,0,11,22
          , 33,0,99,99
          ]

      it "3 bytes aligned to 4 bytes" $ do
        let b = testEncode initialBuffer $ writeTable
                [ writeStructTableField' 4 4 $
                    buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 1 ]
        b `bufferShouldBe`
          [ 12,0,0,0
          , 0,0,6,0
          , 10,0,4,0
          , 6,0,0,0
          , 11,22,33,0
          , 0,0,99,99
          ]

      it "3 bytes aligned to 8 bytes" $ do
        let b = testEncode initialBuffer $ writeTable
                [ writeStructTableField' 8 8 $
                    buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 5 ]
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
        let b = testEncode initialBuffer $ writeTable
                [ writeStructTableField' 16 16 $
                    buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 13 ]
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
        let b = testEncode initialBuffer $ writeTable
                  [ writeVectorTableField $ WriteVectorStruct $ inlineVector id (Alignment 1) (InlineSize 3) 2
                    [ buildWord8 11 <> buildWord8 22 <> buildWord8 33
                    , buildWord8 44 <> buildWord8 55 <> buildWord8 66
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
        let b = testEncode initialBuffer $ writeTable
                  [ writeVectorTableField $ WriteVectorStruct $ inlineVector id (Alignment 2) (InlineSize 4) 2
                    [ buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 1
                    , buildWord8 44 <> buildWord8 55 <> buildWord8 66 <> buildPadding 1
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
        let b = testEncode initialBuffer $ writeTable
                  [ writeVectorTableField $ WriteVectorStruct $ inlineVector id (Alignment 4) (InlineSize 4) 2
                    [ buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 1
                    , buildWord8 44 <> buildWord8 55 <> buildWord8 66 <> buildPadding 1
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
        let b = testEncode initialBuffer $ writeTable
                  [ writeVectorTableField $ WriteVectorStruct $ inlineVector id (Alignment 8) (InlineSize 8) 2
                    [ buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 5
                    , buildWord8 44 <> buildWord8 55 <> buildWord8 66 <> buildPadding 5
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
        let b = testEncode initialBuffer $ writeTable
                  [ writeVectorTableField $ WriteVectorStruct $ inlineVector id (Alignment 16) (InlineSize 16) 2
                    [ buildWord8 11 <> buildWord8 22 <> buildWord8 33 <> buildPadding 13
                    , buildWord8 44 <> buildWord8 55 <> buildWord8 66 <> buildPadding 13
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

