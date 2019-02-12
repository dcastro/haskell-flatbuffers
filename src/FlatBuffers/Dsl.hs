module FlatBuffers.Dsl
  ( Field(..)
  , InlineField(..)
  , root
  , missing
  , vector
  , unionVector
  , bool
  , text
  , int8
  , int16
  , int32
  , int64
  , word8
  , word16
  , word32
  , word64
  , float
  , double
  ) where

import           Data.Bifunctor       (bimap)
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce          (coerce)
import           Data.Int
import           Data.Tagged          (Tagged (..), untag)
import qualified Data.Text            as T
import           Data.Word
import           FlatBuffers          (Field (..), InlineField (..))
import qualified FlatBuffers          as F

root :: Tagged t Field -> BSL.ByteString
root = F.root . untag

missing :: Tagged a Field
missing = Tagged $ Field $ pure $ InlineField 0 0 $ pure ()

-- Here, we specialize `Traversable t` to `[]` so we can use coercions.
-- Otherwise, we'd have to incur the performance penalty of `fmap untag`,
-- because coercions are only safe if the role of `t` is representational.
vector :: [Tagged a Field] -> Tagged [a] Field
vector xs = Tagged $ F.vector (coerce xs :: [Field])

unionVector :: [(Tagged Word8 Field, Tagged a Field)] -> (Tagged [Word8] Field, Tagged [a] Field)
unionVector xs = coerce $ bimap F.vector F.vector $ unzip (coerce xs :: [(Field, Field)])

bool :: Bool -> Tagged Bool Field
bool = Tagged . F.scalar F.bool

-----------------------------------
--- Text
-----------------------------------
text :: T.Text -> Tagged T.Text Field
text = Tagged . F.text

-----------------------------------
--- Int
-----------------------------------
int8 :: Int8 -> Tagged Int8 Field
int8 = Tagged . F.scalar F.int8

int16 :: Int16 -> Tagged Int16 Field
int16 = Tagged . F.scalar F.int16

int32 :: Int32 -> Tagged Int32 Field
int32 = Tagged . F.scalar F.int32

int64 :: Int64 -> Tagged Int64 Field
int64 = Tagged . F.scalar F.int64

-----------------------------------
--- Word
-----------------------------------
word8 :: Word8 -> Tagged Word8 Field
word8 = Tagged . F.scalar F.word8

word16 :: Word16 -> Tagged Word16 Field
word16 = Tagged . F.scalar F.word16

word32 :: Word32 -> Tagged Word32 Field
word32 = Tagged . F.scalar F.word32

word64 :: Word64 -> Tagged Word64 Field
word64 = Tagged . F.scalar F.word64

-----------------------------------
--- Floating point
-----------------------------------
float :: Float -> Tagged Float Field
float = Tagged . F.scalar F.float

double :: Double -> Tagged Double Field
double = Tagged . F.scalar F.double
