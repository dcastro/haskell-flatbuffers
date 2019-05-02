{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module FlatBuffers.Internal.Write
  ( FBState(..)
  , Field(..)
  , InlineField(..)
  , missing
  , table
  , vector
  , struct
  , padded
  , inline
  , root
  , rootWithFileIdentifier
  , word8
  , word16
  , word32
  , word64
  , int8
  , int16
  , int32
  , int64
  , float
  , double
  , bool
  , text
  , byteString
  , prep
  ) where

import           Control.Lens
import           Control.Monad.State

import qualified Data.ByteString            as BS
import           Data.ByteString.Builder    ( Builder )
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as BSL
import           Data.Coerce                ( coerce )
import           Data.Foldable
import qualified Data.Foldable              as Foldable
import           Data.Functor.Reverse       ( Reverse(..) )
import           Data.Int
import qualified Data.List                  as L

import           Data.List.NonEmpty         ( NonEmpty )
import qualified Data.Map.Strict            as M
import           Data.Maybe                 ( fromMaybe )
import           Data.Monoid

import           Data.Semigroup             ( Max(..) )
import           Data.Text                  ( Text )
import qualified Data.Text.Encoding         as T
import           Data.Word

import           FlatBuffers.Constants
import           FlatBuffers.FileIdentifier ( FileIdentifier(..) )
import           FlatBuffers.Internal.Util  ( headF )

type Offset = BytesWritten
type BytesWritten = Int32

data FBState = FBState
  { _builder      :: !Builder
  , _bytesWritten :: !(Sum BytesWritten)
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

inline :: (a -> InlineField) -> (a -> Field)
inline f = Field . pure . f

primitive :: InlineSize -> (a -> Builder) -> a -> InlineField
primitive size f a =
  InlineField size size $ do
    builder %= mappend (f a)
    bytesWritten += fromIntegral size

fileIdentifier :: FileIdentifier -> InlineField
fileIdentifier (unFileIdentifier -> bs) = primitive fileIdentifierSize B.byteString bs

int8 :: Int8 -> InlineField
int8 = primitive int8Size B.int8

int16 :: Int16 -> InlineField
int16 = primitive int16Size B.int16LE

int32 :: Int32 -> InlineField
int32 = primitive int32Size B.int32LE

int64 :: Int64 -> InlineField
int64 = primitive int64Size B.int64LE

word8 :: Word8 -> InlineField
word8 = primitive word8Size B.word8

word16 :: Word16 -> InlineField
word16 = primitive word16Size B.word16LE

word32 :: Word32 -> InlineField
word32 = primitive word32Size B.word32LE

word64 :: Word64 -> InlineField
word64 = primitive word64Size B.word64LE


float :: Float -> InlineField
float = primitive floatSize B.floatLE

double :: Double -> InlineField
double = primitive doubleSize B.doubleLE

bool :: Bool -> InlineField
bool = primitive boolSize $ \case
  True  -> B.word8 1
  False -> B.word8 0

-- | A missing field.
-- Use this when serializing a deprecated field, or to tell clients to use the default value.
missing :: Field
missing = Field . pure . InlineField 0 0 $ pure ()

text :: Text -> Field
text = byteString . T.encodeUtf8

-- | Encodes a bytestring as text.
byteString :: BS.ByteString -> Field
byteString bs = Field $ do
  write $ word8 0 -- trailing zero
  let length = BS.length bs

  prep (coerce uoffsetSize) (fromIntegral length)
  builder %= mappend (B.int32LE (fromIntegral length) <> B.byteString bs)
  Sum bw <- bytesWritten <<>= Sum (fromIntegral @InlineSize @Int32 uoffsetSize + fromIntegral length)
  pure $ uoffsetFrom bw

-- | Prepare to write @n@ bytes after writing @additionalBytes@.
prep :: Word16 -> Word16 {- ^ additionalBytes -} -> State FBState ()
prep n additionalBytes =
  if n == 0
    then pure ()
    else do
      maxAlign <>= Max n
      bw <- uses bytesWritten getSum
      let remainder = (bw + fromIntegral @Word16 @Int32 additionalBytes) `rem` fromIntegral @Word16 @Int32 n
      let needed = if remainder == 0 then 0 else fromIntegral @Word16 @Int32 n - remainder
      sequence_ $ L.genericReplicate needed (write $ word8 0)


root :: Field -> BSL.ByteString
root table =
  B.toLazyByteString $
  _builder $
  execState
    (do ref <- dump table
        align <- uses maxAlign getMax
        prep align (coerce uoffsetSize)
        write ref
    )
    (FBState mempty 0 1 mempty)

rootWithFileIdentifier :: FileIdentifier -> Field -> BSL.ByteString
rootWithFileIdentifier fi table =
  B.toLazyByteString $
  _builder $
  execState
    (do ref <- dump table
        align <- uses maxAlign getMax
        prep align (coerce (uoffsetSize + fileIdentifierSize))
        write $ fileIdentifier fi
        write ref
    )
    (FBState mempty 0 1 mempty)

-- | Fields should be provided in the reverse order as in the schema.
struct :: InlineSize -> NonEmpty InlineField -> Field
struct structAlign fields =
  let structSize = getSum $ foldMap (Sum . size) fields
  in Field . pure . InlineField structSize structAlign $
      traverse_ write fields

-- | Adds zero padding AFTER this field.
padded :: Word8 -> InlineField -> InlineField
padded n field = InlineField (size field + fromIntegral n) (align field + fromIntegral n) $ do
  sequence_ $ L.genericReplicate n (write $ word8 0)
  write field

-- | Fields should be provided in the same order as in the schema.
table :: [Field] -> Field
table fields = Field $
  traverse dump fields >>= table'

vtable :: [InlineSize] -> InlineSize -> BSL.ByteString
vtable fieldOffsets tableSize = bytestring
  where
    vtableSize = voffsetSize + voffsetSize + voffsetSize * L.genericLength fieldOffsets
    bytestring = B.toLazyByteString
      (  B.word16LE (coerce vtableSize)
      <> B.word16LE (coerce tableSize)
      <> foldMap (B.word16LE . coerce) fieldOffsets
      )
      
table' :: [InlineField] -> State FBState InlineField
table' fields = do
  -- table
  tableEnd <- uses bytesWritten getSum
  locations <-
    coerce $ forM fields $ \f ->
      if size f == 0
        then pure 0
        else prep (coerce (align f)) 0 >> write f >> use bytesWritten
  prep (coerce soffsetSize) 0
  tableStart <- uses bytesWritten getSum

  let tableLocation = tableStart + fromIntegral @InlineSize @Int32 4
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
      bytesWritten <>= Sum newVtableSize
    (Just oldVtableLocation, _) ->
      -- pointer to vtable
      write $ int32 $ negate $ fromIntegral (tableLocation - oldVtableLocation)

  pure $ uoffsetFrom tableLocation

-- | NOTE: Assumes all elemenets have the same size and alignment.
vector :: Traversable t => t Field -> Field
vector fields = Field $ do
  inlineFields <- traverse dump fields

  let elemSize = maybe 0 size $ headF inlineFields
  let elemAlign = maybe 0 align $ headF inlineFields
  let elemCount = Foldable.length inlineFields

  prep (coerce uoffsetSize)   (coerce elemSize * fromIntegral @Int @Word16 elemCount)
  prep (coerce elemAlign)     (coerce elemSize * fromIntegral @Int @Word16 elemCount)

  traverse_ write (Reverse inlineFields)
  write (word32 (fromIntegral @Int @Word32 elemCount))
  bw <- uses bytesWritten getSum
  pure $ uoffsetFrom bw

uoffsetFrom :: BytesWritten -> InlineField
uoffsetFrom bw = InlineField uoffsetSize uoffsetSize $ do
  bw2 <- uses bytesWritten getSum
  write (int32 (bw2 - bw + fromIntegral @InlineSize @Int32 uoffsetSize))
