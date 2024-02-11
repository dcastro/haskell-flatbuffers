{-# OPTIONS_HADDOCK not-home #-}

{- HLINT ignore writeTable uoffsetFrom "Eta reduce" -}

module FlatBuffers.Internal.Write where

import Control.Monad.State.Strict

import Data.Bits (complement, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Int
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Monoid (Sum(..))
import Data.MonoTraversable (Element, MonoFoldable)
import Data.MonoTraversable qualified as Mono
import Data.Semigroup (Max(..))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Internal qualified as TI
import Data.Word

import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Text.Lazy qualified as LT
import FlatBuffers.Internal.Build
import FlatBuffers.Internal.Constants
import FlatBuffers.Internal.FileIdentifier
  (FileIdentifier(unFileIdentifier), HasFileIdentifier(getFileIdentifier))
import FlatBuffers.Internal.Types
import Text.Pretty.Simple (pShowNoColor)

showBuffer' :: BS.ByteString -> String
showBuffer' = showBuffer . BSL.fromStrict

showBuffer :: BSL.ByteString -> String
showBuffer bs =
  List.intercalate "\n" . fmap (List.intercalate ", ") . groupsOf 4 . fmap show $
  BSL.unpack bs

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs =
  case take n xs of
    [] -> []
    group -> group : groupsOf n (drop n xs)

prettyPrint :: Show a => a -> PrettyString
prettyPrint a = PrettyString $ pShowNoColor a

newtype PrettyString = PrettyString LT.Text

instance Show PrettyString where
  show (PrettyString text) = LT.unpack text

enc :: WriteTable a -> PrettyString
enc = prettyBuffer . encode

prettyBuffer :: BSL.ByteString -> PrettyString
prettyBuffer = prettyPrint . showBuffer

{-

>>> import qualified FlatBuffers.Internal.Write as F


>>> enc $ F.writeTable [ F.writeInt32TableField 99 ]
"12, 0, 0, 0
0, 0, 6, 0
8, 0, 4, 0
6, 0, 0, 0
99, 0, 0, 0"

>>> enc $ F.writeTable [ F.writeInt32TableField 99, writeTextTableField "abc" ]
"12, 0, 0, 0
8, 0, 12, 0
8, 0, 4, 0
8, 0, 0, 0
8, 0, 0, 0
99, 0, 0, 0
3, 0, 0, 0
97, 98, 99, 0"


 -}

data Person = Person
  { personName :: Text
  , personAge :: Int32
  }

{-
>>> prettyBuffer encodePerson
"16, 0, 0, 0
0, 0, 10, 0
12, 0, 8, 0
0, 0, 4, 0
10, 0, 0, 0
22, 0, 0, 0
11, 0, 0, 0"

-}

encodePerson :: BSL.ByteString
encodePerson =
  encode do
    writeTable
      [
        writeInt32TableField 11
      ,
        optional writeInt32TableField Nothing
      ,
        writeInt32TableField 22
      ]

encodePeople1 :: [Person] -> BSL.ByteString
encodePeople1 people =
  encode do
    writeTable
      [ writeVectorTableTableField $ fromList' $ people <&> \p ->
          writeTable
            [
              writeInt32TableField p.personAge
              ,
              writeTextTableField p.personName
            ]
      ]

{-

>>> people = [Person "aaa" 44, Person "bbb" 55]

>>> BSL.writeFile "1.bin" $ encodePeople1 people

>>> prettyBuffer $ encodePeople1 people
"12, 0, 0, 0
0, 0, 6, 0
8, 0, 4, 0
6, 0, 0, 0
4, 0, 0, 0
2, 0, 0, 0
8, 0, 0, 0
32, 0, 0, 0
236, 255, 255, 255
8, 0, 0, 0
44, 0, 0, 0
3, 0, 0, 0
97, 97, 97, 0
8, 0, 12, 0
8, 0, 4, 0
8, 0, 0, 0
8, 0, 0, 0
55, 0, 0, 0
3, 0, 0, 0
98, 98, 98, 0"

 -}

encodeWeapons :: [Either Text Int32] -> BSL.ByteString
encodeWeapons weapons = do
  encode do
    let vec = fromList' $ weapons <&> \case
          Left str ->
            writeUnion 1 $ writeTable [ writeTextTableField str ]
          Right int ->
            writeUnion 2 $ writeTable [ writeInt32TableField int ]
    writeTable
      [ writeUnionTypesVectorTableField vec
      , writeUnionValuesVectorTableField vec
      ]

{-

>>> BSL.writeFile "weapons1.bin" $ encodeWeapons [Left "aa", Right 11]

flatc --annotate weapons.fbs weapons1.bin

prettyBuffer $ encodeWeapons [Left "aa", Right 11]
"12, 0, 0, 0
8, 0, 12, 0
8, 0, 4, 0
8, 0, 0, 0 -- start of table
8, 0, 0, 0
48, 0, 0, 0
2, 0, 0, 0   -- start of 1 union vec
8, 0, 0, 0
28, 0, 0, 0
238, 255, 255, 255   -- 1st weapon, sword
4, 0, 0, 0
2, 0, 0, 0
97, 97, 0, 0
0, 0, 6, 0
8, 0, 4, 0
6, 0, 0, 0     -- 2nd weapon, axe
11, 0, 0, 0    -- 28 points to here
2, 0, 0, 0     -- start of union type vec
1, 2, 0, 0"

-}

type BufferSize = Sum Int32

-- | The position of something in a buffer, expressed as the number of bytes counting from the end.
type Position = Int32

data FBState = FBState
  { builder    :: !Builder
  , bufferSize :: {-# UNPACK #-} !BufferSize
  , maxAlign   :: {-# UNPACK #-} !(Max Alignment)
  , cache      :: !(M.Map BSL.ByteString Position)
  }

newtype WriteTableField = WriteTableField { unWriteTableField :: State FBState (FBState -> FBState) }

-- | A struct to be written to a flatbuffer.
newtype WriteStruct a = WriteStruct { buildStruct :: Builder }

-- | A table to be written to a flatbuffer.
newtype WriteTable a = WriteTable (State FBState Position)

-- | A union to be written to a flatbuffer.
data WriteUnion a = WriteUnion
  { wuUnionType :: {-# UNPACK #-} !Word8
  , wuUnionValue :: !(State FBState Position)
  }

-- | Serializes a flatbuffer table as a lazy `BSL.ByteString`.
{-# INLINE encode #-}
encode :: WriteTable a -> BSL.ByteString
encode = encodeState (FBState mempty (Sum 0) (Max 1) mempty)

{-# INLINE encodeState #-}
encodeState :: FBState -> WriteTable a -> BSL.ByteString
encodeState state (WriteTable writeTable) =
  B.toLazyByteString $
  builder $
  execState
    (do pos <- writeTable
        maxAlignment <- gets (getMax . maxAlign)
        modify' $ alignTo maxAlignment uoffsetSize
        modify' $ uoffsetFrom pos
    )
    state

-- | Serializes a flatbuffer table as a lazy `BSL.ByteString` and adds a File Identifier.
{-# INLINE encodeWithFileIdentifier #-}
encodeWithFileIdentifier :: forall a. HasFileIdentifier a => WriteTable a -> BSL.ByteString
encodeWithFileIdentifier =
  encodeStateWithFileIdentifier (FBState mempty (Sum 0) (Max 1) mempty) (getFileIdentifier @a)

{-# INLINE encodeStateWithFileIdentifier #-}
encodeStateWithFileIdentifier :: FBState -> FileIdentifier -> WriteTable a -> BSL.ByteString
encodeStateWithFileIdentifier state fi (WriteTable writeTable) =
  B.toLazyByteString $
  builder $
  execState
    (do pos <- writeTable
        maxAlignment <- gets (getMax . maxAlign)
        modify' $ alignTo maxAlignment (uoffsetSize + fileIdentifierSize)
        modify' $ writeFileIdentifier fi
        modify' $ uoffsetFrom pos
    )
    state


-- | Writes something (unaligned) to the buffer.
{-# INLINE write #-}
write :: Int32 -> Builder -> FBState -> FBState
write bsize b fbs = fbs
  { builder = b <> builder fbs
  , bufferSize = bufferSize fbs <> Sum bsize
  }

-- | Writes a 32-bit int (unaligned) to the buffer.
{-# INLINE writeInt32 #-}
writeInt32 :: Int32 -> FBState -> FBState
writeInt32 n = write int32Size (B.int32LE n)

{-# INLINE writeFileIdentifier #-}
writeFileIdentifier :: FileIdentifier -> FBState -> FBState
writeFileIdentifier fi = write fileIdentifierSize (B.byteString (unFileIdentifier fi))

{-# INLINE missing #-}
missing :: WriteTableField
missing = WriteTableField . pure $! id

{-# INLINE deprecated #-}
deprecated :: WriteTableField
deprecated = missing

{-# INLINE optional #-}
optional :: (a -> WriteTableField) -> (Maybe a -> WriteTableField)
optional = maybe missing

{-# INLINE optionalDef #-}
optionalDef :: Eq a => a -> (a -> WriteTableField) -> (Maybe a -> WriteTableField)
optionalDef dflt write ma =
  case ma of
    Just a | a /= dflt -> write a
    _                  -> missing


{-# INLINE writeWord8TableField #-}
writeWord8TableField :: Word8 -> WriteTableField
writeWord8TableField n = WriteTableField . pure $! write word8Size (B.word8 n) . alignTo word8Size 0

{-# INLINE writeWord16TableField #-}
writeWord16TableField :: Word16 -> WriteTableField
writeWord16TableField n = WriteTableField . pure $! write word16Size (B.word16LE n) . alignTo word16Size 0

{-# INLINE writeWord32TableField #-}
writeWord32TableField :: Word32 -> WriteTableField
writeWord32TableField n = WriteTableField . pure $! write word32Size (B.word32LE n) . alignTo word32Size 0

{-# INLINE writeWord64TableField #-}
writeWord64TableField :: Word64 -> WriteTableField
writeWord64TableField n = WriteTableField . pure $! write word64Size (B.word64LE n) . alignTo word64Size 0

{-# INLINE writeInt8TableField #-}
writeInt8TableField :: Int8 -> WriteTableField
writeInt8TableField n = WriteTableField . pure $! write int8Size (B.int8 n) . alignTo int8Size 0

{-# INLINE writeInt16TableField #-}
writeInt16TableField :: Int16 -> WriteTableField
writeInt16TableField n = WriteTableField . pure $! write int16Size (B.int16LE n) . alignTo int16Size 0

{-# INLINE writeInt32TableField #-}
writeInt32TableField :: Int32 -> WriteTableField
writeInt32TableField n = WriteTableField . pure $! write int32Size (B.int32LE n) . alignTo int32Size 0

{-# INLINE writeInt64TableField #-}
writeInt64TableField :: Int64 -> WriteTableField
writeInt64TableField n = WriteTableField . pure $! write int64Size (B.int64LE n) . alignTo int64Size 0

{-# INLINE writeFloatTableField #-}
writeFloatTableField :: Float -> WriteTableField
writeFloatTableField n = WriteTableField . pure $! write floatSize (B.floatLE n) . alignTo floatSize 0

{-# INLINE writeDoubleTableField #-}
writeDoubleTableField :: Double -> WriteTableField
writeDoubleTableField n = WriteTableField . pure $! write doubleSize (B.doubleLE n) . alignTo doubleSize 0

{-# INLINE writeBoolTableField #-}
writeBoolTableField :: Bool -> WriteTableField
writeBoolTableField = writeWord8TableField . boolToWord8

{-# INLINE writeTextTableField #-}
writeTextTableField :: Text -> WriteTableField
writeTextTableField text = WriteTableField $ do
  modify' (writeInt32 len . encodeText . alignTo int32Size (len + 1))
  uoffsetFromHere
  where
    len = utf8length text
    encodeText fbs =
      fbs
        -- strings must have a trailing zero
        { builder = T.encodeUtf8Builder text <> B.word8 0 <> builder fbs
        , bufferSize = Sum len <> Sum 1 <> bufferSize fbs
        }

{-# INLINE writeTableTableField #-}
writeTableTableField :: WriteTable a -> WriteTableField
writeTableTableField (WriteTable writeTable) = WriteTableField $ do
  loc <- writeTable
  pure $! uoffsetFrom loc

{-# INLINE writeStructTableField #-}
writeStructTableField :: forall a. IsStruct a => WriteStruct a -> WriteTableField
writeStructTableField (WriteStruct b) =
  writeStructTableField' (structAlignmentOf @a) (structSizeOf @a) b

{-# INLINE writeStructTableField' #-}
writeStructTableField' :: Alignment -> InlineSize -> Builder -> WriteTableField
writeStructTableField' structAlignment structSize structBuilder =
  WriteTableField . pure $! writeStruct . alignTo structAlignment 0
  where
    writeStruct fbs = fbs
      { builder = structBuilder <> builder fbs
      , bufferSize = bufferSize fbs <> Sum (fromIntegral @InlineSize @Int32 structSize)
      }

{-# INLINE writeUnionTypesVectorTableField #-}
writeUnionTypesVectorTableField :: WriteVector (WriteUnion a) -> WriteTableField
writeUnionTypesVectorTableField (WriteVectorUnion tf _) = tf

{-# INLINE writeUnionValuesVectorTableField #-}
writeUnionValuesVectorTableField :: WriteVector (WriteUnion a) -> WriteTableField
writeUnionValuesVectorTableField (WriteVectorUnion _ tf) = tf


{-# INLINE writeUnionTypeTableField #-}
writeUnionTypeTableField :: WriteUnion a -> WriteTableField
writeUnionTypeTableField wu = writeWord8TableField wu.wuUnionType

{-# INLINE writeUnionValueTableField #-}
writeUnionValueTableField :: WriteUnion a -> WriteTableField
writeUnionValueTableField wu = writeTableTableField (WriteTable wu.wuUnionValue)

{-# INLINE writeUnion #-}
writeUnion :: Word8 -> WriteTable a -> WriteUnion b
writeUnion n (WriteTable st) = WriteUnion n st

{-# INLINE vtable #-}
vtable :: [Word16] -> Word16 -> BSL.ByteString
vtable fieldVOffsets tableSize = bytestring
  where
    vtableSize = voffsetSize + voffsetSize + voffsetSize * fromIntegral @Int @Word16 (L.length fieldVOffsets)
    bytestring = B.toLazyByteString
      (  B.word16LE vtableSize
      <> B.word16LE (coerce tableSize)
      <> foldMap (B.word16LE . coerce) fieldVOffsets
      )


{-# INLINE writeTable #-}
writeTable :: [WriteTableField] -> WriteTable a
writeTable fields = WriteTable $ do

  inlineFields <- sequence (coerce fields)

  -- table
  tableEnd <- gets (getSum . bufferSize)

  inlineFieldPositions <-
    forM inlineFields $ \f -> do
      before <- gets bufferSize
      modify' f
      after <- gets bufferSize
      if after == before
        then pure 0
        else pure (getSum after)

  modify' $ alignTo soffsetSize 0

  -- INFO: position of table (before writing the SOffset)
  tableFieldsPosition <- gets (getSum . bufferSize)

  -- INFO: position of table (after writing the SOffset)
  let tablePosition = tableFieldsPosition + soffsetSize
  -- Note: This might overflow if the table has too many fields
  let tableSize = fromIntegral @Int32 @Word16 $ tablePosition - tableEnd
  let fieldVOffsets = flip fmap inlineFieldPositions $ \case
                  0 ->
                    -- 0 means the field is absent (null or deprecated),
                    -- so we write 0 in the vtable entry.
                    0
                  -- Note: This might overflow if the table has too many fields
                  fieldPosition -> fromIntegral @Int32 @Word16 (tablePosition - fieldPosition)

  -- TODO: trim trailing 0 voffsets

  let newVtable = vtable fieldVOffsets tableSize
  let newVtableSize = fromIntegral @Int64 @Int32 (BSL.length newVtable)
  let newVtablePosition = tablePosition + newVtableSize

  map <- gets cache
  case M.insertLookupWithKey (\_k _new old -> old) newVtable newVtablePosition map of
    (Nothing, map') ->
      -- vtable, pointer to vtable, update the cache
      modify' (writeVtable map' newVtable newVtableSize . writeVtableSoffset newVtableSize)

    (Just oldVtablePosition, _) ->
      -- pointer to vtable
      modify' . writeInt32 . negate $ tablePosition - oldVtablePosition

  pure $! tablePosition

  where
    writeVtable newCache newVtable newVtableSize fbs = fbs
      { cache = newCache
      , builder = B.lazyByteString newVtable <> builder fbs
      , bufferSize = bufferSize fbs <> Sum newVtableSize
      }

    -- The vtable is located right before the table, so the offset
    -- between the table and the vtable is equal to the vtable size
    writeVtableSoffset newVtableSize = writeInt32 newVtableSize



class WriteVectorElement a where

  -- | A vector to be written to a flatbuffer.
  data WriteVector a

  -- | Constructs a flatbuffers vector.
  --
  -- If @n@ is larger than the length of @xs@, this will result in a malformed buffer.
  -- If @n@ is smaller than the length of @xs@, all elements of @xs@ will still be written to the buffer,
  -- but the client will only be able to read the first @n@ elements.
  --
  -- Note: `fromMonoFoldable` asks for the collection's length to be passed in as an argument rather than use `Mono.olength` because:
  --
  -- 1. `Mono.olength` is often O(n), and in some use cases there may be a better way to know the collection's length ahead of time.
  -- 2. Calling `Mono.olength` inside `fromMonoFoldable` can inhibit some fusions which would otherwise be possible.
  --
  -- @since 0.2.0.0


  -- Implementer's note:
  -- To elaborate on point 2., here's an example.
  -- This version of `fromMonoFoldable` that calls `Mono.olength` internally:
  --
  -- > encodeUserIds' :: [User] -> BSL.ByteString
  -- > encodeUserIds' = encode . userIdsTable $ fromMonoFoldable (userId <$> users))
  -- >
  -- > {-# INLINE fromMonoFoldable #-}
  -- > fromMonoFoldable xs =
  -- >   let length = Mono.olength xs
  -- >       buffer = foldr ... ... xs
  -- >   in  ...
  --
  -- ...prevents `<$>` and `foldr` from being fused, and so it's much slower than when the length is passed in:
  --
  -- > encodeUserIds :: [User] -> BSL.ByteString
  -- > encodeUserIds = encode . userIdsTable $ fromMonoFoldable (userId <$> users) (fromIntegral (Mono.olength users))
  -- >
  -- > {-# INLINE fromMonoFoldable #-}
  -- > fromMonoFoldable xs length =
  -- >   let buffer = foldr ... ... xs
  -- >   in  ...
  fromMonoFoldable ::
       (MonoFoldable mono, Element mono ~ a)
    => Int32      -- ^ @n@: the number of elements in @xs@
    -> mono       -- ^ @xs@: a collection
    -> WriteVector a

-- | Convenience function, equivalent to:
--
-- > fromMonoFoldable' xs = fromMonoFoldable (fromIntegral (olength xs)) xs
--
-- In some cases it may be slower than using `fromMonoFoldable` directly.
--
-- @since 0.2.0.0
{-# INLINE fromMonoFoldable' #-}
fromMonoFoldable' :: (WriteVectorElement a, MonoFoldable mono, Element mono ~ a) => mono -> WriteVector a
fromMonoFoldable' xs = fromMonoFoldable (fromIntegral $ Mono.olength xs) xs

-- | `fromMonoFoldable` specialized to list
fromList :: WriteVectorElement a => Int32 -> [a] -> WriteVector a
fromList = fromMonoFoldable

-- | `fromMonoFoldable'` specialized to list
fromList' :: WriteVectorElement a => [a] -> WriteVector a
fromList' = fromMonoFoldable'

-- | Creates a flatbuffers vector with a single element
singleton :: WriteVectorElement a => a -> WriteVector a
singleton a = fromList 1 [a]

-- | Creates an empty flatbuffers vector
empty :: WriteVectorElement a => WriteVector a
empty = fromList 0 []


newtype FromFoldable f a = FromFoldable (f a)
  deriving newtype Foldable

type instance Element (FromFoldable f a) = a
instance Foldable f => MonoFoldable (FromFoldable f a)

-- | `fromMonoFoldable` for types that implement `Foldable` but not `MonoFoldable`.
fromFoldable :: (WriteVectorElement a, Foldable f) => Int32 -> f a -> WriteVector a
fromFoldable n = fromMonoFoldable n . FromFoldable

-- | `fromMonoFoldable'` for types that implement `Foldable` but not `MonoFoldable`.
fromFoldable' :: (WriteVectorElement a, Foldable f) => f a -> WriteVector a
fromFoldable' = fromMonoFoldable' . FromFoldable

-- | Efficiently creates a vector from a `BS.ByteString`.
-- Large `BS.ByteString`s are inserted directly, but small ones are copied to ensure that the generated chunks are large on average.
--
-- @since 0.2.0.0
fromByteString :: BS.ByteString -> WriteVector Word8
fromByteString bs = WriteVectorWord8 . WriteTableField $ do
  modify' $!
    writeInt32 len . writeByteString . alignTo int32Size len
  uoffsetFromHere
  where
    len = fromIntegral @Int @Int32 (BS.length bs)
    writeByteString fbs =
      fbs
        { builder = B.byteString bs <> builder fbs
        , bufferSize = bufferSize fbs <> Sum len
        }

-- | Efficiently creates a vector from a lazy `BSL.ByteString`.
--  Large chunks of the `BSL.ByteString` are inserted directly, but small ones are copied to ensure that the generated chunks are large on average.
--
-- @since 0.2.0.0
fromLazyByteString :: BSL.ByteString -> WriteVector Word8
fromLazyByteString bs = WriteVectorWord8 . WriteTableField $ do
  modify' $!
    writeInt32 len . writeByteString . alignTo int32Size len
  uoffsetFromHere
  where
    len = fromIntegral @Int64 @Int32 (BSL.length bs)
    writeByteString fbs =
      fbs
        { builder = B.lazyByteString bs <> builder fbs
        , bufferSize = bufferSize fbs <> Sum len
        }

{-# INLINE inlineVector #-}
inlineVector ::
     (MonoFoldable mono, Element mono ~ a)
  => (a -> Builder)
  -> Alignment
  -> InlineSize
  -> Int32
  -> mono
  -> WriteTableField
inlineVector build elemAlignment elemSize elemCount elems = WriteTableField $ do
  modify' $!
    writeInt32 elemCount . writeVec . alignTo (coerce elemAlignment `max` int32Size) vecByteLength

  uoffsetFromHere
  where
    vecByteLength = elemCount * fromIntegral @InlineSize @Int32 elemSize
    vecBuilder = Mono.ofoldr (\a b -> build a <> b) mempty elems
    writeVec fbs =
      fbs
        { builder = vecBuilder <> builder fbs
        , bufferSize = bufferSize fbs <> Sum vecByteLength
        }

instance WriteVectorElement Word8 where
  newtype WriteVector Word8 = WriteVectorWord8 { writeVectorWord8TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Word8) => Int32 -> mono -> WriteVector Word8
  fromMonoFoldable n = WriteVectorWord8 . inlineVector B.word8 word8Size word8Size n

instance WriteVectorElement Word16 where
  newtype WriteVector Word16 = WriteVectorWord16 { writeVectorWord16TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Word16) => Int32 -> mono -> WriteVector Word16
  fromMonoFoldable n = WriteVectorWord16 . inlineVector B.word16LE word16Size word16Size n

instance WriteVectorElement Word32 where
  newtype WriteVector Word32 = WriteVectorWord32 { writeVectorWord32TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Word32) => Int32 -> mono -> WriteVector Word32
  fromMonoFoldable n = WriteVectorWord32 . inlineVector B.word32LE word32Size word32Size n

instance WriteVectorElement Word64 where
  newtype WriteVector Word64 = WriteVectorWord64 { writeVectorWord64TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Word64) => Int32 -> mono -> WriteVector Word64
  fromMonoFoldable n = WriteVectorWord64 . inlineVector B.word64LE word64Size word64Size n

instance WriteVectorElement Int8 where
  newtype WriteVector Int8 = WriteVectorInt8 { writeVectorInt8TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Int8) => Int32 -> mono -> WriteVector Int8
  fromMonoFoldable n = WriteVectorInt8 . inlineVector B.int8 int8Size int8Size n

instance WriteVectorElement Int16 where
  newtype WriteVector Int16 = WriteVectorInt16 { writeVectorInt16TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Int16) => Int32 -> mono -> WriteVector Int16
  fromMonoFoldable n = WriteVectorInt16 . inlineVector B.int16LE int16Size int16Size n

instance WriteVectorElement Int32 where
  newtype WriteVector Int32 = WriteVectorInt32 { writeVectorInt32TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Int32) => Int32 -> mono -> WriteVector Int32
  fromMonoFoldable n = WriteVectorInt32 . inlineVector B.int32LE int32Size int32Size n

instance WriteVectorElement Int64 where
  newtype WriteVector Int64 = WriteVectorInt64 { writeVectorInt64TableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Int64) => Int32 -> mono -> WriteVector Int64
  fromMonoFoldable n = WriteVectorInt64 . inlineVector B.int64LE int64Size int64Size n

instance WriteVectorElement Float where
  newtype WriteVector Float = WriteVectorFloat { writeVectorFloatTableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Float) => Int32 -> mono -> WriteVector Float
  fromMonoFoldable n = WriteVectorFloat . inlineVector B.floatLE floatSize floatSize n

instance WriteVectorElement Double where
  newtype WriteVector Double = WriteVectorDouble { writeVectorDoubleTableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Double) => Int32 -> mono -> WriteVector Double
  fromMonoFoldable n = WriteVectorDouble . inlineVector B.doubleLE doubleSize doubleSize n

instance WriteVectorElement Bool where
  newtype WriteVector Bool = WriteVectorBool { writeVectorBoolTableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Bool) => Int32 -> mono -> WriteVector Bool
  fromMonoFoldable n = WriteVectorBool . inlineVector (B.word8 . boolToWord8) word8Size word8Size n

instance IsStruct a => WriteVectorElement (WriteStruct a) where
  newtype WriteVector (WriteStruct a) = WriteVectorStruct { writeVectorStructTableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ WriteStruct a) => Int32 -> mono -> WriteVector (WriteStruct a)
  fromMonoFoldable n = WriteVectorStruct . inlineVector coerce (structAlignmentOf @a) (structSizeOf @a) n


data TextInfos = TextInfos ![TextInfo] {-# UNPACK #-} !BufferSize

data TextInfo = TextInfo
  { tiText     :: !Text
  , tiUtf8len  :: {-# UNPACK #-} !Int32
  , tiPadding  :: {-# UNPACK #-} !Int32
  , tiPosition :: {-# UNPACK #-} !Position
  }

data OffsetInfo = OffsetInfo
  { oiIndex   :: {-# UNPACK #-} !Int32
  , oiOffsets :: ![Int32]
  }

instance WriteVectorElement Text where
  newtype WriteVector Text = WriteVectorText { writeVectorTextTableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ Text) => Int32 -> mono -> WriteVector Text
  fromMonoFoldable elemCount texts = WriteVectorText . WriteTableField $ do
    modify' $ \fbs ->
      let (builder2, bsize2) =
            writeVectorSizePrefix . writeOffsets . align . writeStrings $ (builder fbs, bufferSize fbs)
      in  fbs
            { builder = builder2
            , bufferSize = bsize2
            , maxAlign = maxAlign fbs <> Max int32Size
            }
    uoffsetFromHere
    where
      writeStrings :: (Builder, BufferSize) -> (Builder, BufferSize, [TextInfo])
      writeStrings (builder1, bsize1) =
          -- Collect info about the strings.
          -- NOTE: this loop *could* be merged with the one below, but
          -- we have loops dedicated to merging Builders to avoid wrapping Builders in data structures.
          -- See "Performance tips": http://hackage.haskell.org/package/fast-builder-0.1.0.1/docs/Data-ByteString-FastBuilder.html
        let TextInfos textInfos bsize2 =
              Mono.ofoldr
                (\t (TextInfos infos bsize) ->
                  let textLength = utf8length t
                      padding = calcPadding 4 (textLength + 1) bsize
                      newBsize = bsize <> Sum (padding + textLength + 1 + 4)
                  in  TextInfos (TextInfo t textLength padding (getSum newBsize) : infos) newBsize
                )
                (TextInfos [] bsize1)
                texts

            builder2 =
              foldr
                (\(TextInfo t tlength padding _) b ->
                  B.int32LE tlength
                  <> T.encodeUtf8Builder t
                  <> B.word8 0 -- strings must have a trailing zero
                  <> buildPadding padding
                  <> b
                )
                mempty
                textInfos
        in (builder2 <> builder1, bsize2, textInfos)

      align :: (Builder, BufferSize, [TextInfo]) -> (Builder, BufferSize, [TextInfo])
      align (builder1, bsize1, textInfos) =
        let vectorPadding = calcPadding int32Size 0 bsize1
            bsize2 = bsize1 <> Sum vectorPadding
            builder2 = buildPadding vectorPadding
        in  (builder2 <> builder1, bsize2, textInfos)

      writeOffsets :: (Builder, BufferSize, [TextInfo]) -> (Builder, BufferSize)
      writeOffsets (builder1, bsize1, textInfos) =
        let OffsetInfo _ offsets =
              foldr
                (\(TextInfo _ _ _ position) (OffsetInfo ix os) ->
                  OffsetInfo
                    (ix + 1)
                    (getSum bsize1 + (ix * 4) + 4 - position : os)
                )
                (OffsetInfo 0 [])
                textInfos

            bsize2 = bsize1 <> Sum (elemCount * 4)
            builder2 =
              foldr
                (\o b -> B.int32LE o <> b)
                mempty
                offsets
        in  (builder2 <> builder1, bsize2)

      writeVectorSizePrefix :: (Builder, BufferSize) -> (Builder, BufferSize)
      writeVectorSizePrefix (builder1, bsize1) =
        (B.int32LE elemCount <> builder1, bsize1 + int32Size)



data TableInfo = TableInfo
  { tiState          :: !FBState
  , tiTablePositions :: ![Position]
  }

instance WriteVectorElement (WriteTable a) where
  newtype WriteVector (WriteTable a) = WriteVectorTable { writeVectorTableTableField :: WriteTableField }

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ WriteTable a) => Int32 -> mono -> WriteVector (WriteTable a)
  fromMonoFoldable elemCount tables = WriteVectorTable . WriteTableField $ do
    fbs1 <- get
    let !(TableInfo fbs2 positions) =
          Mono.ofoldr
            (\(WriteTable writeTable) (TableInfo fbs positions) ->
              let (pos, fbs') = runState writeTable fbs
              in  TableInfo fbs' (pos : positions)
            )
            (TableInfo fbs1 [])
            tables
    put $! alignTo int32Size 0 fbs2

    -- Write offsets
    bsize <- gets (getSum . bufferSize)
    let OffsetInfo _ offsets =
          foldr
            (\position (OffsetInfo ix os) ->
              OffsetInfo
                (ix + 1)
                (bsize + (ix * 4) + 4 - position : os)
            )
            (OffsetInfo 0 [])
            positions

    coerce $ fromMonoFoldable elemCount offsets

data Vecs a = Vecs ![Word8] ![State FBState Position]

data UnionTableInfo = UnionTableInfo
  { utiState          :: !FBState
  , utiTablePositions :: ![Position]
  }

instance WriteVectorElement (WriteUnion a) where
  data WriteVector (WriteUnion a) = WriteVectorUnion !WriteTableField !WriteTableField

  {-# INLINE fromMonoFoldable #-}
  fromMonoFoldable :: (MonoFoldable mono, Element mono ~ WriteUnion a) => Int32 -> mono -> WriteVector (WriteUnion a)
  fromMonoFoldable elemCount unions =
    let Vecs types values =
          Mono.ofoldr
            go
            (Vecs [] [])
            unions

        go :: WriteUnion a -> Vecs a -> Vecs a
        go writeUnion (Vecs types values) =
          Vecs (writeUnion.wuUnionType : types) (writeUnion.wuUnionValue : values)

        writeUnionTables :: WriteTableField
        writeUnionTables = WriteTableField $ do
              fbs1 <- get
              let !(UnionTableInfo fbs2 positions) =
                    foldr
                      (\unionTable (UnionTableInfo fbs positions) ->
                          let (pos, fbs') = runState unionTable fbs
                          in  UnionTableInfo fbs' (pos : positions)
                      )
                      (UnionTableInfo fbs1 [])
                      values
              put $! alignTo int32Size 0 fbs2


              -- Write offsets
              bsize <- gets (getSum . bufferSize)
              let OffsetInfo _ offsets =
                    foldr
                      (\position (OffsetInfo ix os) ->
                        let offset = bsize + (ix * 4) + 4 - position
                        in  OffsetInfo
                              (ix + 1)
                              (offset : os)
                      )
                      (OffsetInfo 0 [])
                      positions

              coerce $ fromMonoFoldable elemCount offsets

    in  WriteVectorUnion (coerce $ fromMonoFoldable elemCount types) writeUnionTables

-- | Calculate how much 0-padding is needed so that, after writing @additionalBytes@,
-- the buffer becomes aligned to @n@ bytes.
{-# INLINE calcPadding #-}
calcPadding :: Alignment {- ^ n -} -> Int32 {- ^ additionalBytes -} -> BufferSize -> Int32
calcPadding !n !additionalBytes (Sum size) =
  (complement (size + additionalBytes) + 1) .&. (fromIntegral n - 1)

-- | Add enough 0-padding so that the buffer becomes aligned to @n@ after writing @additionalBytes@.
{-# INLINE alignTo #-}
alignTo :: Alignment{- ^ n -} -> Int32 {- ^ additionalBytes -} -> FBState -> FBState
alignTo !n !additionalBytes fbs@(FBState b bsize ma cache) =
  if padding == 0
    then fbs { maxAlign = ma <> coerce n }
    else FBState
            (buildPadding padding <> b)
            (bsize <> Sum padding)
            (ma <> coerce n)
            cache
  where
    padding = calcPadding n additionalBytes bsize


{-# INLINE uoffsetFromHere #-}
uoffsetFromHere :: State FBState (FBState -> FBState)
uoffsetFromHere = gets (uoffsetFrom . coerce . bufferSize)

{-# INLINE uoffsetFrom #-}
uoffsetFrom :: Position -> FBState -> FBState
uoffsetFrom pos = writeUOffset . align
  where
    align fbs = alignTo int32Size 0 fbs
    writeUOffset fbs =
      let currentPos = coerce bufferSize fbs
      in  writeInt32 (currentPos - pos + uoffsetSize) fbs

{-# INLINE utf8length #-}
utf8length :: Text -> Int32
utf8length (TI.Text _array _offset len) = fromIntegral @Int @Int32 len
