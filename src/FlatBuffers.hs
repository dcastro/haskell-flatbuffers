{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

module FlatBuffers where

import           Control.Monad.State
import qualified Data.ByteString           as BS
import           Data.ByteString.Builder   (Builder, toLazyByteString)
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8      as BSU
import           Data.Int
import qualified Data.List                 as L
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.Word
import           Debug.Trace


type BytesWritten = Int
type Offset = BytesWritten
type InlineSize = Word16
type BState = (Builder, BytesWritten)

newtype Field = Field { dump :: State BState InlineField }

newtype InlineField = InlineField { write :: State BState InlineSize }

referenceSize :: Num a => a
referenceSize = 4

scalar' :: InlineField -> Field
scalar' = Field . pure

scalar :: (a -> InlineField) -> (a -> Field)
scalar f = scalar' . f

primitive :: InlineSize -> (a -> Builder) -> a -> InlineField
primitive size f a =
  InlineField $ do
    (b, bw) <- get
    put (f a <> b, bw + fromIntegral size)
    pure size


int32 :: Int32 -> InlineField
int32 = primitive 4 B.int32LE

int64 :: Int64 -> InlineField
int64 = primitive 8 B.int64LE

word8 :: Word8 -> InlineField
word8 = primitive 1 B.word8

float :: Float -> InlineField
float = primitive 4 B.floatLE

double :: Double -> InlineField
double = primitive 4 B.doubleLE

bool :: Bool -> InlineField
bool = primitive 1 $ \case
  True  -> B.word8 1
  False -> B.word8 0

-- | A missing field.
-- | Use this when serializing a deprecated field, or to tell clients to use the default value.
missing :: Field
missing = Field $ pure missing'

missing' :: InlineField
missing' = InlineField $ pure 0

lazyText :: TL.Text -> Field
lazyText = lazyByteString . TL.encodeUtf8

text :: T.Text -> Field
text = byteString . T.encodeUtf8

string :: String -> Field
string = lazyByteString . BSLU.fromString

string' :: String -> State BState InlineField
string' = dump . string

-- | Encodes a bytestring as text.
byteString :: BS.ByteString -> Field
byteString bs = Field $ do
  (b, bw) <- get
  let length = BS.length bs
  let (b2, bw2) = (B.int32LE (fromIntegral length) <> B.byteString bs <> b, bw + referenceSize + fromIntegral length)
  put (b2, bw2)
  pure $ offsetFrom bw2

-- | Encodes a lazy bytestring as text.
lazyByteString :: BSL.ByteString -> Field
lazyByteString bs = Field $ do
  (b, bw) <- get
  let length = BSL.length bs
  let (b2, bw2) = (B.int32LE (fromIntegral length) <> B.lazyByteString bs <> b, bw + referenceSize + fromIntegral length)
  put (b2, bw2)
  pure $ offsetFrom bw2

root :: [Field] -> Builder
root fields =
  fst $ execState
    (dump (table fields) >>= write)
    (mempty, 0)

runRoot :: State BState () -> Builder
runRoot state =
  fst $ execState
    state
    (mempty, 0)

root' :: [InlineField] -> State BState ()
root' fields = void $ table' fields >>= write

struct :: [InlineField] -> InlineField
struct fields = InlineField $
  sum <$> traverse write (reverse fields)


padded :: Word16 -> InlineField -> InlineField
padded n field = InlineField $ do
  sequence_ $ L.genericReplicate n (write $ word8 0)
  size <- write field
  pure (size + n)

table :: [Field] -> Field
table fields = Field $ do
  inlineFields <- traverse dump (reverse fields)
  table' (reverse inlineFields)

table' :: [InlineField] -> State BState InlineField
table' fields = do
  inlineSizes <- traverse write (reverse fields)

  let fieldOffsets = calcFieldOffsets referenceSize (reverse inlineSizes)
  let vtableSize = 2 + 2 + 2 * L.genericLength fields
  let tableSize = referenceSize + fromIntegral (sum inlineSizes)

  (b, bw)  <- get

  -- table
  let (b2, bw2) = (B.int32LE vtableSize <> b, bw + 4)

  -- vtable
  let (b3, bw3) = (B.word16LE vtableSize <> B.word16LE tableSize <> foldMap B.word16LE fieldOffsets <> b2, bw2 + vtableSize)
  
  put (b3, bw3)
  pure $ offsetFrom bw2

vector :: [Field] -> Field
vector fields = Field $ do
  inlineFields <- traverse dump (reverse fields)
  inlineSizes <- traverse write inlineFields
  write (int32 (L.genericLength fields))
  (_, bw) <- get
  pure $ offsetFrom bw

offsetFrom :: BytesWritten -> InlineField
offsetFrom bw = InlineField $ do
  (_, bw2) <- get
  write (int32 (fromIntegral (bw2 - bw) + referenceSize))

calcFieldOffsets :: Word16 -> [InlineSize] -> [Word16]
calcFieldOffsets seed []     = []
calcFieldOffsets seed (0:xs) = 0 : calcFieldOffsets seed xs
calcFieldOffsets seed (x:xs) = seed : calcFieldOffsets (seed + x) xs
