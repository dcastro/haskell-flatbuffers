{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Data.Semigroup            (Max (..))
import           Data.Tagged               (Tagged, untag)
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
  , _bytesWritten :: !BytesWritten
  , _maxAlign     :: !(Max Word16)
  , _cache        :: !(M.Map BSL.ByteString Offset)
  }

makeLenses ''FBState

newtype Field = Field { dump :: State FBState InlineField }

data InlineField = InlineField
  { size :: !InlineSize
  , align :: !InlineSize
  , write :: !(State FBState ())
  }

referenceSize :: Num a => a
referenceSize = 4

scalar' :: InlineField -> Field
scalar' = Field . pure

scalar :: (a -> InlineField) -> (a -> Field)
scalar f = scalar' . f

primitive :: InlineSize -> (a -> Builder) -> a -> InlineField
primitive size f a =
  InlineField size size $ do
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

word16 :: Word16 -> InlineField
word16 = primitive 2 B.word16LE

word32 :: Word32 -> InlineField
word32 = primitive 4 B.word32LE

word64 :: Word64 -> InlineField
word64 = primitive 8 B.word64LE


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
missing' = InlineField 0 0 $ pure ()

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
prep n additionalBytes =
  if n == 0
    then pure ()
    else do
      maxAlign <>= Max n
      bw <- gets _bytesWritten
      let remainder = (fromIntegral bw + additionalBytes) `rem` n
      let needed = if remainder == 0 then 0 else n - remainder
      sequence_ $ L.genericReplicate needed (write $ word8 0)

rootT :: Tagged t Field -> BSL.ByteString
rootT = root . untag

root :: Field -> BSL.ByteString
root table =
  B.toLazyByteString $
  _builder $
  execState
    (do ref <- dump table
        root' ref)
    (FBState mempty 0 (Max 1) mempty)

runRoot :: State FBState () -> Builder
runRoot state =
  _builder $ execState
    state
    (FBState mempty 0 (Max 1) mempty)

root' :: InlineField -> State FBState ()
root' ref = do
  align <- uses maxAlign getMax
  prep align referenceSize
  write ref

struct :: [InlineField] -> InlineField
struct fields = InlineField (getSum $ foldMap (Sum . size) fields) (getMax $ foldMap (Max . align) fields) $
  traverse_ write (reverse fields)

padded :: Word8 -> InlineField -> InlineField
padded n field = InlineField (size field + fromIntegral n) (align field + fromIntegral n) $ do
  sequence_ $ L.genericReplicate n (write $ word8 0)
  write field

table :: [Field] -> Field
table fields = Field $
  traverse dump fields >>= table'

vtable :: [InlineSize] -> InlineSize -> BSL.ByteString
vtable fieldOffsets tableSize = bytestring
  where
    vtableSize = 2 + 2 + 2 * L.genericLength fieldOffsets
    bytestring = B.toLazyByteString
      (  B.word16LE vtableSize
      <> B.word16LE tableSize
      <> foldMap B.word16LE fieldOffsets
      )
      
table' :: [InlineField] -> State FBState InlineField
table' fields = do
  -- table
  tableEnd <- gets _bytesWritten
  locations <-
    fmap reverse $ forM (reverse fields) $ \f ->
      if size f == 0
        then pure 0
        else prep (align f) 0 >> write f >> gets _bytesWritten
  prep referenceSize 0
  tableStart <- gets _bytesWritten

  let tableLocation = tableStart + referenceSize
  let tableSize = tableLocation - tableEnd
  let fieldOffsets = flip fmap locations $ \case
                  0 -> 0
                  loc -> fromIntegral (tableLocation - loc)

  let newVtable = vtable fieldOffsets (fromIntegral tableSize)
  let newVtableSize = fromIntegral (BSL.length newVtable)
  let newVtableLocation = tableLocation + newVtableSize

  map <- gets _cache
  case M.insertLookupWithKey (\k new old -> old) newVtable newVtableLocation map of
    (Nothing, map') -> do
      -- update the cache
      cache .= map'

      -- pointer to vtable
      write $ int32 (fromIntegral newVtableSize)
      
      -- vtable
      builder %= mappend (B.lazyByteString newVtable)
      bytesWritten += newVtableSize
    (Just oldVtableLocation, _) ->
      -- pointer to vtable
      write $ int32 $ negate $ fromIntegral (tableLocation - oldVtableLocation)

  pure $ uoffsetFrom tableLocation

vector :: [Field] -> Field
vector fields = Field $ do
  inlineFields <- traverse dump fields

  -- TODO: all elements should have the same size
  let elemSize = getMax $ foldMap (Max . size) inlineFields
  let elemAlign = getMax $ foldMap (Max . align) inlineFields
  let elemCount = L.genericLength inlineFields

  prep 4 (elemSize * elemCount)
  prep elemAlign (elemSize * elemCount)
  
  traverse_ write (reverse inlineFields)
  write (int32 (L.genericLength fields))
  bw <- gets _bytesWritten
  pure $ uoffsetFrom bw

uoffsetFrom :: BytesWritten -> InlineField
uoffsetFrom bw = InlineField referenceSize referenceSize $ do
  bw2 <- gets _bytesWritten
  write (int32 (fromIntegral (bw2 - bw) + referenceSize))
