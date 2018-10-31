{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}


module FlatBuffers where

import           Control.Lens
import           Control.Monad.State
import qualified Data.ByteString           as BS
import           Data.ByteString.Builder   (Builder)
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8      as BSU
import           Data.Foldable
import           Data.Int
import qualified Data.List                 as L
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.Word
import           Debug.Trace


type InlineSize = Word16
type Offset = BytesWritten
type BytesWritten = Int

data FBState = FBState
  { _builder      :: !Builder
  , _bytesWritten :: !Int
  , _cache        :: !(M.Map BSL.ByteString Offset)
  }

makeLenses ''FBState

newtype Field = Field { dump :: State FBState InlineField }

data InlineField = InlineField { size :: !InlineSize, write :: !(State FBState ()) }

referenceSize :: Num a => a
referenceSize = 4

scalar' :: InlineField -> Field
scalar' = Field . pure

scalar :: (a -> InlineField) -> (a -> Field)
scalar f = scalar' . f

primitive :: InlineSize -> (a -> Builder) -> a -> InlineField
primitive size f a =
  InlineField size $ do
    builder %= mappend (f a)
    bytesWritten += fromIntegral size

int8 :: Int8 -> InlineField
int8 = primitive 1 B.int8

int16 :: Int16 -> InlineField
int16 = primitive 2 B.int16LE

int32 :: Int32 -> InlineField
int32 = primitive 4 B.int32LE

int64 :: Int64 -> InlineField
int64 = primitive 8 B.int64LE

word8 :: Word8 -> InlineField
word8 = primitive 1 B.word8

word32 :: Word32 -> InlineField
word32 = primitive 4 B.word32LE


float :: Float -> InlineField
float = primitive 4 B.floatLE

double :: Double -> InlineField
double = primitive 8 B.doubleLE

bool :: Bool -> InlineField
bool = primitive 1 $ \case
  True  -> B.word8 1
  False -> B.word8 0

-- | A missing field.
-- Use this when serializing a deprecated field, or to tell clients to use the default value.
missing :: Field
missing = Field $ pure missing'

missing' :: InlineField
missing' = InlineField 0 $ pure ()

lazyText :: TL.Text -> Field
lazyText = lazyByteString . TL.encodeUtf8

text :: T.Text -> Field
text = byteString . T.encodeUtf8

string :: String -> Field
string = lazyByteString . BSLU.fromString

string' :: String -> State FBState InlineField
string' = dump . string

-- | Encodes a bytestring as text.
byteString :: BS.ByteString -> Field
byteString bs = Field $ do
  write $ word8 0 -- trailing zero
  let length = BS.length bs

  prep referenceSize (fromIntegral length)
  builder %= mappend (B.int32LE (fromIntegral length) <> B.byteString bs)
  bw <- bytesWritten <+= referenceSize + fromIntegral length
  pure $ uoffsetFrom bw

-- | Encodes a lazy bytestring as text.
lazyByteString :: BSL.ByteString -> Field
lazyByteString bs = Field $ do
  write $ word8 0 -- trailing zero
  let length = BSL.length bs

  prep referenceSize (fromIntegral length)
  builder %= mappend (B.int32LE (fromIntegral length) <> B.lazyByteString bs)
  bw <- bytesWritten <+= referenceSize + fromIntegral length
  pure $ uoffsetFrom bw

-- | Prepare to write @n@ bytes after writing @additionalBytes@.
prep :: Word16 -> Word16 {- ^ additionalBytes -} -> State FBState ()
prep n additionalBytes = do
  bw <- gets _bytesWritten
  let needed = calcPadding (bw + fromIntegral additionalBytes) n
  sequence_ $ L.genericReplicate needed (write $ word8 0)

root :: [Field] -> Builder
root fields =
  _builder $ execState
    (dump (table fields) >>= write)
    (FBState mempty 0 mempty)

runRoot :: State FBState () -> Builder
runRoot state =
  _builder $ execState
    state
    (FBState mempty 0 mempty)

root' :: [InlineField] -> State FBState ()
root' fields = void $ table' fields >>= write

struct :: [InlineField] -> InlineField
struct fields = InlineField (getSum $ foldMap (Sum . size) fields) $
  traverse_ write (reverse fields)


padded :: Word8 -> InlineField -> InlineField
padded n field = InlineField (size field + fromIntegral n) $ do
  sequence_ $ L.genericReplicate n (write $ word8 0)
  write field

table :: [Field] -> Field
table fields = Field $
  traverse dump fields >>= table'
  
vtable :: [InlineSize] -> Field
vtable inlineSizes = Field $ do
  
  let (_, tableHeaderAlignedSize : alignedSizes) = foldr calcAlignedSizes (0, []) (referenceSize : inlineSizes)
  let fieldOffsets = calcFieldOffsets tableHeaderAlignedSize alignedSizes

  let vtableSize = 2 + 2 + 2 * L.genericLength inlineSizes
  let tableSize = referenceSize + sum inlineSizes

  let bytestring = B.toLazyByteString
        (  B.word16LE vtableSize
        <> B.word16LE tableSize
        <> foldMap B.word16LE fieldOffsets
        )
  bw <- (+ vtableSize) <$> gets _bytesWritten
  
  map <- gets _cache

  case M.insertLookupWithKey (\k new old -> old) bytestring bw map of
    (Nothing, map') -> do
      builder %= mappend (B.lazyByteString bytestring)
      bytesWritten += vtableSize
      cache .= map'
      pure $ soffsetFrom bw

    (Just oldBw, _) ->
      pure $ soffsetFrom oldBw
      
table' :: [InlineField] -> State FBState InlineField
table' fields = do
  -- vtable
  let inlineSizes = map size fields
  vtableRef <- dump $ vtable inlineSizes

  -- table
  traverse_ (\f -> prep (size f) 0 >> write f) (reverse fields)
  prep referenceSize 0 >> write vtableRef
  bw <- gets _bytesWritten

  pure $ uoffsetFrom bw

vector :: [Field] -> Field
vector fields = Field $ do
  inlineFields <- traverse dump fields
  traverse_ write (reverse inlineFields)
  write (int32 (L.genericLength fields))
  bw <- gets _bytesWritten
  pure $ uoffsetFrom bw

uoffsetFrom :: BytesWritten -> InlineField
uoffsetFrom bw = InlineField referenceSize $ do
  bw2 <- gets _bytesWritten
  write (int32 (fromIntegral (bw2 - bw) + referenceSize))

soffsetFrom :: BytesWritten -> InlineField
soffsetFrom bw = InlineField referenceSize $ do
  bw2 <- gets _bytesWritten
  write (int32 (negate (fromIntegral (bw2 - bw) + referenceSize)))

calcFieldOffsets :: Word16 -> [InlineSize] -> [Word16]
calcFieldOffsets seed []     = []
calcFieldOffsets seed (0:xs) = 0 : calcFieldOffsets seed xs
calcFieldOffsets seed (x:xs) = seed : calcFieldOffsets (seed + x) xs

calcAlignedSizes :: InlineSize -> (Word16, [InlineSize]) -> (Word16, [InlineSize])
calcAlignedSizes size (totalSize, sizes) =
  (totalSize + alignedSize, alignedSize : sizes)
  where
    alignedSize = size + calcPadding (fromIntegral totalSize) size

calcPadding :: BytesWritten -> InlineSize -> InlineSize
calcPadding bw n = if n == 0 || remainder == 0 then 0 else n - remainder
  where
    remainder = fromIntegral bw `rem` n
