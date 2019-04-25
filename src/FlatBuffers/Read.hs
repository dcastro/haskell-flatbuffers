{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}

module FlatBuffers.Read
  ( ReadCtx
  , TableIndex(..), VectorIndex(..)
  , FieldName(..), VectorLength(..)
  , VOffset(..)
  , ReadError(..)
  , Struct(..)
  , Table(..)
  , HasPosition(..)
  , Position(..)
  , PositionInfo(..)
  , Vector(..)
  , RawVector(..)
  , Union(..)
  , decode
  , checkFileIdentifier, checkFileIdentifier'
  , readWord8, readWord16, readWord32, readWord64
  , readInt8, readInt16, readInt32, readInt64
  , readBool, readFloat, readDouble
  , readText
  , readTable
  , readVector
  , readStruct
  , readStruct'
  , readStructField
  , readTableFieldOpt
  , readTableFieldReq
  , readTableFieldWithDef
  , readTableFieldUnion
  , readTableFieldUnionVectorOpt
  , readTableFieldUnionVectorReq
  , vectorLength, index, toList
  ) where

import           Control.Exception.Safe        ( Exception, MonadThrow, throwM )

import           Data.Binary.Get               ( Get )
import qualified Data.Binary.Get               as G
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Unsafe        as BSU
import           Data.ByteString.Lazy          ( ByteString )
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.Coerce                   ( Coercible, coerce )
import           Data.Functor                  ( (<&>) )
import           Data.Int
import           Data.String                   ( IsString )
import           Data.Text                     ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import           Data.Word

import           FlatBuffers.Constants
import           FlatBuffers.FileIdentifier    ( FileIdentifier(..), HasFileIdentifier(..) )
import           FlatBuffers.Internal.Positive ( Positive(getPositive), positive )

import           HaskellWorks.Data.Int.Widen   ( widen16, widen32, widen64 )

type ReadCtx m = MonadThrow m

newtype FieldName = FieldName Text
  deriving newtype (Show, Eq, IsString)

newtype TableIndex = TableIndex { unTableIndex :: Word16 }
  deriving newtype (Show, Num)

newtype VectorLength = VectorLength { unVectorLength :: Word32 }
  deriving newtype (Show, Num, Eq)

newtype VectorIndex = VectorIndex { unVectorIndex :: Word32 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

newtype UOffset = UOffset { unUOffset :: Word32 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

newtype OffsetFromRoot = OffsetFromRoot { unOffsetFromRoot :: Word64 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

data Table = Table
  { vtable   :: !ByteString
  , tablePos :: !PositionInfo
  }

newtype Struct = Struct { unStruct :: Position }

data Vector a
  = Vector
      !(RawVector a)                                  -- ^ A pointer to an actual FlatBuffers vector
      !(forall m. ReadCtx m => PositionInfo -> m a)   -- ^ A function to read elements from this vector
  | forall b. (a ~ Union b) => UnionVector
      !ByteString                                     -- ^ A byte-vector, where each byte represents the type of each "union value" in the vector
      !(RawVector (Union b))                          -- ^ A table vector, with the actual union values
      !(forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union b)) -- ^ A function to read a union value from this vector

data RawVector a = RawVector
  { rawVectorLength   :: !VectorLength
  , rawVectorPos      :: !PositionInfo
  , rawVectorElemSize :: !InlineSize
  }

data Union a
  = Union !a
  | UnionNone
  | UnionUnknown !Word8


type Position = ByteString

-- | Current position in the buffer
data PositionInfo = PositionInfo
  { posRoot           :: !ByteString      -- ^ Pointer to the buffer root
  , posCurrent        :: !Position        -- ^ Pointer to current position
  , posOffsetFromRoot :: !OffsetFromRoot  -- ^ Number of bytes between current position and root
  }

class HasPosition a where
  getPosition :: a -> Position

instance HasPosition ByteString   where getPosition = id
instance HasPosition PositionInfo where getPosition = posCurrent

decode :: forall t m. (ReadCtx m, Coercible Table t) => ByteString -> m t
decode root = readTable initialPos
  where
    initialPos = PositionInfo root root 0

-- | Checks if a buffer contains the file identifier for a root table @a@, to see if it's
-- safe to decode it to a table @a@.
-- It should be used in conjunction with @-XTypeApplications@.
-- 
-- > {-# LANGUAGE TypeApplications #-}
-- >
-- > if checkFileIdentifier @Monster bs
-- >   then decode @Monster bs
-- >   else return someMonster
checkFileIdentifier :: forall a. HasFileIdentifier a => ByteString -> Bool
checkFileIdentifier = checkFileIdentifier' (getFileIdentifier @a)

checkFileIdentifier' :: FileIdentifier -> ByteString -> Bool
checkFileIdentifier' (unFileIdentifier -> fileIdent) bs =
  actualFileIdent == BSL.fromStrict fileIdent
  where
    actualFileIdent =
      BSL.take (fromIntegral @InlineSize @Int64 fileIdentifierSize) .
        BSL.drop (fromIntegral @InlineSize @Int64 uoffsetSize) $
          bs

----------------------------------
----- Read from Struct/Table -----
----------------------------------
readStructField :: (Coercible s Struct) => (Position -> a) -> VOffset -> s -> a
readStructField read voffset (coerce -> Struct bs) =
  read (move' bs (fromIntegral @VOffset @Int64 voffset))

readTableFieldOpt :: (Coercible t Table, ReadCtx m) => (PositionInfo -> m a) -> TableIndex -> t -> m (Maybe a)
readTableFieldOpt read ix (coerce -> t :: Table) = do
  mbOffset <- tableIndexToVOffset t ix
  traverse (\offset -> read (moveV (tablePos t) offset)) mbOffset

readTableFieldReq :: (Coercible t Table, ReadCtx m) => (PositionInfo -> m a) -> TableIndex -> FieldName -> t -> m a
readTableFieldReq read ix name (coerce -> t :: Table) = do
  mbOffset <- tableIndexToVOffset t ix
  case mbOffset of
    Nothing -> throwM $ MissingField name
    Just offset -> read (moveV (tablePos t) offset)

readTableFieldWithDef :: (ReadCtx m, Coercible t Table) => (PositionInfo -> m a) -> TableIndex -> a -> t -> m a
readTableFieldWithDef read ix dflt (coerce -> t :: Table) =
  tableIndexToVOffset t ix >>= \case
    Nothing -> pure dflt
    Just offset -> read (moveV (tablePos t) offset)

readTableFieldUnion :: (Coercible t Table, ReadCtx m) => (Positive Word8 -> PositionInfo -> m (Union a)) -> TableIndex -> t -> m (Union a)
readTableFieldUnion read ix (coerce -> t :: Table) =
  readTableFieldWithDef readWord8 ix 0 t >>= \unionType ->
    case positive unionType of
      Nothing         -> pure UnionNone
      Just unionType' -> 
        tableIndexToVOffset t (ix + 1) >>= \case
          Nothing     -> throwM $ MalformedBuffer "Union: 'union type' found but 'union value' is missing."
          Just offset -> read unionType' (moveV (tablePos t) offset)

readTableFieldUnionVectorOpt :: (Coercible t Table, ReadCtx m)
  => (forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union a))
  -> TableIndex
  -> t
  -> m (Maybe (Vector (Union a)))
readTableFieldUnionVectorOpt read ix (coerce -> t :: Table) =
  tableIndexToVOffset t ix >>= \case
    Nothing -> pure Nothing
    Just typesOffset ->
      tableIndexToVOffset t (ix + 1) >>= \case
        Nothing -> throwM $ MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          Just <$> readUnionVector read (moveV (tablePos t) typesOffset) (moveV (tablePos t) valuesOffset)

readTableFieldUnionVectorReq :: (Coercible t Table, ReadCtx m)
  => (forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union a))
  -> TableIndex
  -> FieldName
  -> t
  -> m (Vector (Union a))
readTableFieldUnionVectorReq read ix name (coerce -> t :: Table) =
  tableIndexToVOffset t ix >>= \case
    Nothing -> throwM $ MissingField name
    Just typesOffset ->
      tableIndexToVOffset t (ix + 1) >>= \case
        Nothing -> throwM $ MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          readUnionVector read (moveV (tablePos t) typesOffset) (moveV (tablePos t) valuesOffset)

----------------------------------
------- Vector functions ---------
----------------------------------

-- | If the index is too large, this might read garbage data, or fail with a `ReadError`.
index :: forall a m. ReadCtx m => Vector a -> VectorIndex -> m a
index v n =
  case v of
    Vector vec readElem' ->
      readElemRaw readElem' n vec
    UnionVector types values readUnion -> do
      unionType <- byteStringSafeIndex types (fromIntegral @VectorIndex @Int64 n)
      case positive unionType of
        Nothing         -> pure UnionNone
        Just unionType' -> readElemRaw (readUnion unionType') n values
  where
    readElemRaw :: forall a m. ReadCtx m => (PositionInfo -> m a) -> VectorIndex -> RawVector a -> m a
    readElemRaw readElem n vec =
      readElem elemPos
      where
        elemSize = fromIntegral @InlineSize @Int64 (rawVectorElemSize vec)
        elemOffset = 4 + (fromIntegral @VectorIndex @Int64 n * elemSize)
        elemPos = move (rawVectorPos vec) elemOffset

toList :: forall a m. ReadCtx m => Vector a -> m [a]
toList vec =
  if vectorLength vec == 0
    then pure []
    else traverse (\i -> vec `index` i) [0 .. coerce (vectorLength vec) - 1]

vectorLength :: Vector a -> VectorLength
vectorLength (Vector v _)        = rawVectorLength v
vectorLength (UnionVector _ t _) = rawVectorLength t -- NOTE: we assume the two vectors have the same length

----------------------------------
------ Read from `Position` ------
----------------------------------
readInt8 :: (ReadCtx m, HasPosition a) => a -> m Int8
readInt8 (getPosition -> pos) = runGetM G.getInt8 pos

readInt16 :: (ReadCtx m, HasPosition a) => a -> m Int16
readInt16 (getPosition -> pos) = runGetM G.getInt16le pos

readInt32 :: (ReadCtx m, HasPosition a) => a -> m Int32
readInt32 (getPosition -> pos) = runGetM G.getInt32le pos

readInt64 :: (ReadCtx m, HasPosition a) => a -> m Int64
readInt64 (getPosition -> pos) = runGetM G.getInt64le pos

readWord8 :: (ReadCtx m, HasPosition a) => a -> m Word8
readWord8 (getPosition -> pos) = runGetM G.getWord8 pos

readWord16 :: (ReadCtx m, HasPosition a) => a -> m Word16
readWord16 (getPosition -> pos) = runGetM G.getWord16le pos

readWord32 :: (ReadCtx m, HasPosition a) => a -> m Word32
readWord32 (getPosition -> pos) = runGetM G.getWord32le pos

readWord64 :: (ReadCtx m, HasPosition a) => a -> m Word64
readWord64 (getPosition -> pos) = runGetM G.getWord64le pos

readFloat :: (ReadCtx m, HasPosition a) => a -> m Float
readFloat (getPosition -> pos) = runGetM G.getFloatle pos

readDouble :: (ReadCtx m, HasPosition a) => a -> m Double
readDouble (getPosition -> pos) = runGetM G.getDoublele pos

readBool :: (ReadCtx m, HasPosition a) => a -> m Bool
readBool p = toBool <$> readWord8 p
  where
    toBool 0 = False
    toBool _ = True

readVector ::
     forall a m. ReadCtx m
  => (forall m. ReadCtx m => PositionInfo -> m a)
  -> InlineSize
  -> PositionInfo
  -> m (Vector a)
readVector readElem' elemSize pos =
  fmap (\rv -> Vector rv readElem') (readRawVector elemSize pos)

readUnionVector ::
     forall a m. ReadCtx m
  => (forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union a))
  -> PositionInfo
  -> PositionInfo
  -> m (Vector (Union a))
readUnionVector readUnion typesPos valuesPos =
  do
    typesVecOffset <- readWord32 typesPos
    let typesVec = move' (posCurrent typesPos) (4 + fromIntegral @Word32 @Int64 typesVecOffset)
    valuesVec <- readRawVector tableSize valuesPos
    pure $ UnionVector typesVec valuesVec readUnion

readRawVector ::
     forall a m. ReadCtx m
  => InlineSize
  -> PositionInfo
  -> m (RawVector a)
readRawVector elemSize pos@PositionInfo{..} =
  flip runGetM posCurrent $ do
    uoffset <- readAndSkipUOffset
    length <- G.getWord32le
    pure $ RawVector
      { rawVectorLength = VectorLength length
      , rawVectorPos = moveU pos uoffset
      , rawVectorElemSize = elemSize
      }

readText :: ReadCtx m => PositionInfo -> m Text
readText PositionInfo{..} = do
  bs <- flip runGetM posCurrent $ do
    readAndSkipUOffset
    strLength <- G.getWord32le
    -- NOTE: this might overflow in systems where Max Int < Max Word32
    G.getByteString $ fromIntegral @Word32 @Int strLength
  case T.decodeUtf8' bs of
    Right t -> pure t
    Left (T.DecodeError msg b) -> throwM $ Utf8DecodingError (T.pack msg) b
    -- The `EncodeError` constructor is deprecated and not used
    -- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Encoding-Error.html#t:UnicodeException
    Left _ -> error "the impossible happened"

-- | Convenience function for reading structs from table fields / vectors
readStruct' :: (Applicative f, Coercible Struct t, HasPosition a) => a -> f t
readStruct' = pure . readStruct

readStruct :: (Coercible Struct t, HasPosition a) => a -> t
readStruct (getPosition -> pos) = coerce (Struct pos)

readTable :: forall t m. (ReadCtx m, Coercible Table t) => PositionInfo -> m t
readTable pos@PositionInfo{..} =
  flip runGetM posCurrent $ do
    tableOffset <- readAndSkipUOffset
    soffset <- G.getInt32le

    let tableOffset64 = fromIntegral @UOffset @Int64 tableOffset
    let tableOffsetFromRoot = tableOffset64 + fromIntegral @_ @Int64 posOffsetFromRoot
    let vtable = BSL.drop (tableOffsetFromRoot - widen64 soffset) posRoot
    pure . coerce $ Table vtable (moveU pos tableOffset)


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

moveV :: PositionInfo -> VOffset -> PositionInfo
moveV pos offset = move pos (fromIntegral @VOffset @Int64 offset)

moveU :: PositionInfo -> UOffset -> PositionInfo
moveU pos offset = move pos (fromIntegral @UOffset @Int64 offset)

move :: PositionInfo -> Int64 -> PositionInfo
move PositionInfo{..} offset =
  PositionInfo
  { posRoot = posRoot
  , posCurrent = move' posCurrent offset
  , posOffsetFromRoot = posOffsetFromRoot + fromIntegral @Int64 @OffsetFromRoot offset
  }

move' :: Position -> Int64 -> ByteString
move' bs offset = BSL.drop offset bs

readAndSkipUOffset :: Get UOffset
readAndSkipUOffset = do
  uoffset <- G.getWord32le
  -- NOTE: this might overflow in systems where Max Int < Max Word32
  G.skip (fromIntegral @Word32 @Int uoffset - 4)
  pure (UOffset uoffset)

data ReadError
  = ParsingError { position :: !G.ByteOffset
                 , msg      :: !Text }
  | MissingField { fieldName :: !FieldName }
  | Utf8DecodingError { msg  :: !Text
                      , byte :: !(Maybe Word8) }
  | MalformedBuffer !Text
  deriving (Show, Eq)

instance Exception ReadError

runGetM :: ReadCtx m => Get a -> ByteString -> m a
runGetM get =
  feedAll (G.runGetIncremental get)
  where
    feedAll (G.Done _ _ x) _ = pure x
    feedAll (G.Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
    feedAll (G.Fail _ pos msg) _ = throwM $ ParsingError pos (T.pack msg)

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
        
-- Adapted from `Data.ByteString.Lazy.index`: https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.Lazy.html#index
-- Assumes i >= 0.
byteStringSafeIndex :: ReadCtx m => ByteString -> Int64 -> m Word8
byteStringSafeIndex !cs0 !i =
  -- NOTE: this might overflow in systems where Max Int < Max Int64
  index' cs0 i
  where index' BSL.Empty _ = throwM $ MalformedBuffer "Buffer has fewer bytes than indicated by the vector length"
        index' (BSL.Chunk c cs) n
          | n >= fromIntegral @Int @Int64 (BS.length c) =
              index' cs (n - fromIntegral @Int @Int64 (BS.length c))
          | otherwise = pure $! BSU.unsafeIndex c (fromIntegral @Int64 @Int n)
