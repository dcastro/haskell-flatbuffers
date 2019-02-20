{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module FlatBuffers.Read
  ( ReadCtx
  , Error(..)
  , Struct
  , Table
  , Position
  , Vector
  , decode
  , word8Size, word16Size, word32Size, word64Size
  , int8Size, int16Size, int32Size, int64Size
  , boolSize, floatSize, doubleSize
  , textSize, tableSize
  , readWord8, readWord16, readWord32, readWord64
  , readInt8, readInt16, readInt32, readInt64
  , readBool, readFloat, readDouble
  , readText
  , readTable
  , readVector
  , readStruct
  , readStructField
  , readTableField
  , readTableFieldWithDef
  , readTableFieldUnion
  , readTableFieldUnionVector
  , vectorLength, readElem, toList
  , req, opt
  ) where
  
import           Control.Exception.Safe        (Exception, MonadThrow, throwM)
import           Data.Binary.Get               (Get)
import qualified Data.Binary.Get               as G
import qualified Data.ByteString               as BS
import           Data.ByteString.Lazy          (ByteString)
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.Coerce                   (Coercible, coerce)
import           Data.Functor                  ((<&>))
import           Data.Int
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import           Data.Word
import           FlatBuffers.Internal.Write    (InlineSize)
import           HaskellWorks.Data.Int.Widen   (widen16, widen32, widen64)

type ReadCtx m = MonadThrow m

newtype FieldName = FieldName Text
  deriving (Show, Eq, IsString)

newtype TableIndex = TableIndex { unTableIndex :: Word16 }
  deriving (Show, Num)

newtype VectorLength = VectorLength { unVectorLength :: Word32 }
  deriving (Show, Num, Eq)

newtype VectorIndex = VectorIndex { unVectorIndex :: Word32 }
  deriving (Show, Num, Real, Ord, Enum, Integral, Eq)

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving (Show, Num, Real, Ord, Enum, Integral, Eq)

newtype UOffset = UOffset { unUOffset :: Word32 }
  deriving (Show, Num, Real, Ord, Enum, Integral, Eq)

newtype OffsetFromRoot = OffsetFromRoot { unOffsetFromRoot :: Word64 }
  deriving (Show, Num, Real, Ord, Enum, Integral, Eq)

data Table = Table
  { vtable   :: !ByteString
  , tablePos :: !Position
  }

newtype Struct = Struct { unStruct :: Position }

data Vector a
  = Vector
      !(RawVector a)                              -- ^ A pointer to an actual FlatBuffers vector
      !(forall m. ReadCtx m => Position -> m a)   -- ^ A function to read elements from this vector
  | UnionVector
      !(RawVector Word8) -- ^ A byte-vector, where each byte represents the type of each "union value" in the vector
      !(RawVector a)     -- ^ A table vector, with the actual union values
      !a                 -- ^ A value that represents the `None` value for the union type `a`
      !(forall m. ReadCtx m => Word8 -> Position -> m a) -- ^ A function to read a union value from this vector

data RawVector a = RawVector
  { rawVectorLength   :: !VectorLength
  , rawVectorPos      :: !Position
  , rawVectorElemSize :: !InlineSize
  }


-- | Current position in the buffer
data Position = Position
  { posRoot           :: !ByteString -- ^ Pointer to the buffer root
  , posCurrent        :: !ByteString -- ^ Pointer to current position
  , posOffsetFromRoot :: !OffsetFromRoot -- ^ Number of bytes between current position and root
  }

class HasPosition a where
  getPos :: a -> Position

instance HasPosition Table      where getPos = tablePos
instance HasPosition Struct     where getPos = unStruct
instance HasPosition (RawVector a) where getPos = rawVectorPos

decode :: forall t m. (ReadCtx m, Coercible Table t) => ByteString -> m t
decode root = readTable initialPos
  where
    initialPos = Position root root 0

----------------------------------
------------- ReadMode -----------
---------------------------------- 
newtype ReadMode a b = 
  ReadMode (forall m. ReadCtx m => FieldName -> Maybe a -> m b)

req :: ReadMode a a
req = ReadMode req'
  where
    req' _ (Just a) = pure a
    req' fn _       = throwM $ MissingField fn

opt :: ReadMode a (Maybe a)
opt = ReadMode (const pure)


----------------------------------
----- Read from Struct/Table -----
----------------------------------
readStructField :: (Coercible s Struct) => (Position -> a) -> VOffset -> s -> a
readStructField read voffset (coerce -> s :: Struct) =
  read (move s voffset)

readTableField :: (Coercible t Table, ReadCtx m) => (Position -> m a) -> TableIndex -> FieldName -> ReadMode a b -> t -> m b
readTableField read ix name (ReadMode mode) (coerce -> t :: Table) = do
  mbOffset <- tableIndexToVOffset t ix
  case mbOffset of
    Nothing -> mode name Nothing
    Just offset -> read (move t offset) >>= \a -> mode name (Just a)

readTableFieldWithDef :: (ReadCtx m, Coercible t Table) => (Position -> m a) -> TableIndex -> a -> t -> m a
readTableFieldWithDef read ix dflt (coerce -> t :: Table) =
  tableIndexToVOffset t ix >>= \case
    Nothing -> pure dflt
    Just offset -> read (move t offset)

readTableFieldUnion :: (Coercible t Table, ReadCtx m) => (Word8 -> Position -> m a) -> TableIndex -> a -> t -> m a
readTableFieldUnion read ix none t =
  readTableFieldWithDef readWord8 ix 0 t >>= \case
    0          -> pure none
    uniontType -> readTableFieldWithDef (read uniontType) (ix + 1) none t 

readTableFieldUnionVector :: (Coercible t Table, ReadCtx m)
  => (forall m. ReadCtx m => Word8 -> Position -> m a)
  -> TableIndex
  -> FieldName
  -> a
  -> ReadMode (Vector a) b
  -> t
  -> m b
readTableFieldUnionVector read ix name none (ReadMode mode) (coerce -> t :: Table) =
  tableIndexToVOffset t ix >>= \case
    Nothing -> mode name Nothing
    Just typesOffset ->
      tableIndexToVOffset t (ix + 1) >>= \case
        Nothing -> mode name Nothing
        Just valuesOffset -> do
          vec <- readUnionVector none read (move t typesOffset) (move t valuesOffset)
          mode name (Just vec)


----------------------------------
-------------- Sizes -------------
----------------------------------
referenceSize :: InlineSize
referenceSize = 4

textSize, tableSize :: InlineSize
textSize = referenceSize
tableSize = referenceSize

word8Size, word16Size, word32Size, word64Size :: InlineSize
(word8Size, word16Size, word32Size, word64Size) = (1, 2, 4, 8)

int8Size, int16Size, int32Size, int64Size :: InlineSize
(int8Size, int16Size, int32Size, int64Size) = (1, 2, 4, 8)

boolSize, floatSize, doubleSize :: InlineSize
(boolSize, floatSize, doubleSize) = (1, 4, 8)

----------------------------------
------- Vector functions ---------
----------------------------------
readElem :: forall a m. ReadCtx m => VectorIndex -> Vector a -> m a
readElem n v =
  case v of
    Vector vec readElem' ->
      readElemRaw readElem' n vec
    UnionVector types values unionNone readUnion -> do
      unionType <- readElemRaw readWord8 n types
      case unionType of
        0 -> pure unionNone
        _ -> readElemRaw (readUnion unionType) n values
  where
    readElemRaw :: forall a m. ReadCtx m => (Position -> m a) -> VectorIndex -> RawVector a -> m a
    readElemRaw readElem n vec =
      if unVectorIndex n >= unVectorLength (rawVectorLength vec)
        then throwM $ VectorIndexOutOfBounds (rawVectorLength vec) n
        else readElem elemPos
      where
        elemSize = fromIntegral @Word16 @Int64 (rawVectorElemSize vec)
        elemOffset = 4 + (fromIntegral @VectorIndex @Int64 n * elemSize)
        elemPos = moveInt64 vec elemOffset

toList :: forall a m. ReadCtx m => Vector a -> m [a]
toList vec = traverse (\i -> readElem i vec) [0 .. coerce (vectorLength vec) - 1]

vectorLength :: Vector a -> VectorLength
vectorLength (Vector v _         ) = rawVectorLength v
vectorLength (UnionVector v _ _ _) = rawVectorLength v -- NOTE: we assume the two vectors have the same length

----------------------------------
------ Read from `Position` ------
----------------------------------
readInt8 :: ReadCtx m => Position -> m Int8
readInt8 Position{..} = runGetM G.getInt8 posCurrent

readInt16 :: ReadCtx m => Position -> m Int16
readInt16 Position{..} = runGetM G.getInt16le posCurrent

readInt32 :: ReadCtx m => Position -> m Int32
readInt32 Position{..} = runGetM G.getInt32le posCurrent

readInt64 :: ReadCtx m => Position -> m Int64
readInt64 Position{..} = runGetM G.getInt64le posCurrent

readWord8 :: ReadCtx m => Position -> m Word8
readWord8 Position{..} = runGetM G.getWord8 posCurrent

readWord16 :: ReadCtx m => Position -> m Word16
readWord16 Position{..} = runGetM G.getWord16le posCurrent

readWord32 :: ReadCtx m => Position -> m Word32
readWord32 Position{..} = runGetM G.getWord32le posCurrent

readWord64 :: ReadCtx m => Position -> m Word64
readWord64 Position{..} = runGetM G.getWord64le posCurrent

readFloat :: ReadCtx m => Position -> m Float
readFloat Position{..} = runGetM G.getFloatle posCurrent

readDouble :: ReadCtx m => Position -> m Double
readDouble Position{..} = runGetM G.getDoublele posCurrent

readBool :: ReadCtx m => Position -> m Bool
readBool p = toBool <$> readWord8 p
  where
    toBool 0 = False
    toBool _ = True

readVector ::
     forall a m. ReadCtx m
  => (forall m. ReadCtx m => Position -> m a)
  -> InlineSize
  -> Position
  -> m (Vector a)
readVector readElem' elemSize pos =
  fmap (\rv -> Vector rv readElem') (readRawVector elemSize pos)

readUnionVector ::
     forall a m. ReadCtx m
  => a
  -> (forall m. ReadCtx m => Word8 -> Position -> m a)
  -> Position
  -> Position
  -> m (Vector a)
readUnionVector unionNone readUnion typesPos valuesPos =
  do
    typesVec <- readRawVector word8Size typesPos
    valuesVec <- readRawVector tableSize valuesPos
    pure $ UnionVector typesVec valuesVec unionNone readUnion

readRawVector ::
     forall a m. ReadCtx m
  => InlineSize
  -> Position
  -> m (RawVector a)
readRawVector elemSize Position{..} =
  flip runGetM posCurrent $ do
    uoffset <- moveUOffset
    length <- G.getWord32le
    pure $ RawVector
      { rawVectorLength = VectorLength length
      , rawVectorPos =
          Position
          { posRoot = posRoot
          , posCurrent = BSL.drop (fromIntegral @UOffset @Int64 uoffset) posCurrent
          , posOffsetFromRoot = posOffsetFromRoot + fromIntegral @UOffset @OffsetFromRoot uoffset
          }
      , rawVectorElemSize = elemSize
      }

readText :: ReadCtx m => Position -> m Text
readText Position{..} = do
  bs <- flip runGetM posCurrent $ do
    moveUOffset
    strLength <- G.getWord32le
    G.getByteString $ fromIntegral @Word32 @Int strLength
  case T.decodeUtf8' bs of
    Right t -> pure t
    Left (T.DecodeError msg b) -> throwM $ Utf8DecodingError msg b
    -- The `EncodeError` constructor is deprecated and not used
    -- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Encoding-Error.html#t:UnicodeException
    Left _ -> error "the impossible happened"

readStruct :: Coercible Struct t => Position -> t
readStruct = coerce . Struct

readTable :: forall t m. (ReadCtx m, Coercible Table t) => Position -> m t
readTable Position{..} =
  flip runGetM posCurrent $ do
    tableOffset <- moveUOffset
    soffset <- G.getInt32le

    let tableOffset64 = fromIntegral @UOffset @Int64 tableOffset
    let tableOffsetFromRoot = tableOffset64 + fromIntegral @_ @Int64 posOffsetFromRoot
    let vtable = BSL.drop (tableOffsetFromRoot - widen64 soffset) posRoot
    let table = BSL.drop tableOffsetFromRoot posRoot
    pure . coerce $ Table vtable (Position posRoot table (posOffsetFromRoot + fromIntegral @UOffset @OffsetFromRoot tableOffset))


----------------------------------
---------- Primitives ------------
----------------------------------
tableIndexToVOffset :: (ReadCtx m, Coercible t Table) => t -> TableIndex -> m (Maybe VOffset)
tableIndexToVOffset (coerce -> Table{..}) ix =
  flip runGetM vtable $ do
    vtableSize <- G.getWord16le
    let vtableIndex = 4 + (unTableIndex ix * 2)
    if vtableIndex >= vtableSize
      then pure Nothing
      else do
        G.skip (fromIntegral @Word16 @Int vtableIndex - 2)
        G.getWord16le <&> \case
          0 -> Nothing
          word16 -> Just (VOffset word16)

move :: HasPosition p => p -> VOffset -> Position
move hs offset =
  moveInt64 hs (fromIntegral @VOffset @Int64 offset)
  
moveUOffset :: Get UOffset
moveUOffset = do
  uoffset <- G.getWord32le
  G.skip (fromIntegral @Word32 @Int uoffset - 4)
  pure (UOffset uoffset)

moveInt64 :: HasPosition p => p -> Int64 -> Position
moveInt64 (getPos -> Position{..}) offset =
  Position
  { posRoot = posRoot
  , posCurrent = BSL.drop offset posCurrent
  , posOffsetFromRoot = posOffsetFromRoot + fromIntegral @Int64 @OffsetFromRoot offset
  }


data Error
  = ParsingError { position :: G.ByteOffset
                 , msg      :: String }
  | MissingField { fieldName :: FieldName }
  | Utf8DecodingError { msg  :: String
                      , byte :: Maybe Word8 }
  | VectorIndexOutOfBounds VectorLength VectorIndex
  | EnumUnknown { enumName :: String, enumValue :: Word64 }
  | UnionUnknown { unionName :: String, unionValue :: Word8 }
  deriving (Show, Eq)

instance Exception Error

runGetM :: ReadCtx m => Get a -> ByteString -> m a
runGetM get =
  feedAll (G.runGetIncremental get)
  where
    feedAll (G.Done _ _ x) _ = pure x
    feedAll (G.Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
    feedAll (G.Fail _ pos msg) _ = throwM $ ParsingError pos msg

    takeHeadChunk :: BSL.ByteString -> Maybe BS.ByteString
    takeHeadChunk lbs =
      case lbs of
        (BSL.Chunk bs _) -> Just bs
        _ -> Nothing

    dropHeadChunk :: BSL.ByteString -> BSL.ByteString
    dropHeadChunk lbs =
      case lbs of
        (BSL.Chunk _ lbs') -> lbs'
        _ -> BSL.Empty
        