{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Flatbuffers where

import Control.Monad.State
import Data.Bifunctor
import Data.Int
import Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L
import Debug.Trace
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder, toLazyByteString)

go =
  putStrLn "Dumping" >>
  BSL.writeFile "bs.txt" (toLazyByteString $ root vectors)

vectors =
  table [
    vector [text "hi", string "bye"],
    vector [int32 12, int32 34],
    vector [int64 23, int64 45]
  ]

variety =
  table [
    word8 8,
    word8 2,
    table [
      int32 670
    ],
    bool True
  ]

obj = table [
    int32 123,
    text "hello",
    int64 999,
    text "bye",
    int32 456
  ]

nested = 
  table [
    int32 12399,
    table [
      int32 99456,
      text "byehello"
    ]
  ]

type BytesWritten = Int
type Offset = BytesWritten
type InlineSize = Word16
type BState = (Builder, BytesWritten)

newtype Field = Field { dump :: State BState InlineField }

newtype InlineField = InlineField { write :: State BState InlineSize }

referenceSize :: Num a => a
referenceSize = 4

int32 :: Int32 -> Field
int32 = primitive 4 B.int32LE

int64 :: Int64 -> Field
int64 = primitive 8 B.int64LE

word8 :: Word8 -> Field
word8 = primitive 1 B.word8

bool :: Bool -> Field
bool = primitive 1 $ \case
  True  -> B.word8 1
  False -> B.word8 0

primitive :: InlineSize -> (a -> Builder) -> a -> Field
primitive size f a =
  Field . pure . InlineField $ do
    (b, bw) <- get
    put (f a <> b, bw + fromIntegral size)
    pure size

-- | A missing field.
-- | Use this when serializing a deprecated field, or to tell clients to use the default value.
missing :: Field
missing = Field . pure . InlineField $ pure 0

text :: Text -> Field
text t = Field $ do
  (b, bw) <- get
  let (b2, bw2) = (B.int32LE (fromIntegral (T.length t)) <> T.encodeUtf8Builder t <> b, bw + referenceSize + T.length t)
  put (b2, bw2)
  pure $ InlineField $ do
    (b3, bw3) <- get
    let bw4 = bw3 + referenceSize
    let b4 = B.int32LE (fromIntegral (bw4 - bw2)) <> b3
    put (b4, bw4)
    pure referenceSize

string :: String -> Field
string s = Field $ do
  (b, bw) <- get
  let (b2, bw2) = (B.int32LE (L.genericLength s) <> B.stringUtf8 s <> b, bw + referenceSize + length s)
  put (b2, bw2)
  pure $ InlineField $ do
    (b3, bw3) <- get
    let bw4 = bw3 + referenceSize
    let b4 = B.int32LE (fromIntegral (bw4 - bw2)) <> b3
    put (b4, bw4)
    pure referenceSize

root :: Field -> Builder
root field =
  fst $ execState
    (dump field >>= write)
    (mempty, 0)



table :: [Field] -> Field
table fields = Field $ do
  inlineFields <- traverse dump (reverse fields)
  inlineSizes <- traverse write inlineFields


  let fieldOffsets = calcFieldOffsets referenceSize (reverse inlineSizes)
  let vtableSize = 2 + 2 + 2 * L.genericLength fields
  let tableSize = referenceSize + fromIntegral (sum inlineSizes)

  (b, bw)  <- get

  -- table
  let (b2, bw2) = (B.int32LE vtableSize <> b, bw + 4)

  -- vtable
  let (b3, bw3) = (B.word16LE vtableSize <> B.word16LE tableSize <> foldMap B.word16LE fieldOffsets <> b2, bw2 + vtableSize)
  
  put (b3, bw3)
  
  pure $ InlineField $ do
    (b4, bw4) <- get
    let bw5 = bw4 + referenceSize
    let b5 = B.int32LE (fromIntegral (bw5 - bw2)) <> b4
    put (b5, bw5)
    pure referenceSize

vector :: [Field] -> Field
vector fields = Field $ do
  inlineFields <- traverse dump (reverse fields)
  inlineSizes <- traverse write inlineFields
  dump (int32 $ L.genericLength fields) >>= write
  (_, bw) <- get
  pure $ InlineField $
    offsetFrom bw >>= dump . int32 >>= write

offsetFrom :: BytesWritten -> State BState Int32
offsetFrom bw = do
  (_, bw2) <- get
  pure (fromIntegral (bw2 - bw) + referenceSize)

calcFieldOffsets :: Word16 -> [InlineSize] -> [Word16]
calcFieldOffsets seed [] = []
calcFieldOffsets seed (0 : xs) = 0 : calcFieldOffsets seed xs
calcFieldOffsets seed (x : xs) = seed : calcFieldOffsets (seed + x) xs


