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
{-# LANGUAGE TypeFamilies #-}

module FlatBuffers.Read
  ( ReadCtx
  , TableIndex(..)
  , FieldName(..)
  , VOffset(..)
  , ReadError(..)
  , Struct(..)
  , Table(..)
  , HasPosition(..)
  , Position(..)
  , PositionInfo(..)
  , Vector(..), VectorElement(..)
  , Union(..)
  , decode
  , checkFileIdentifier, checkFileIdentifier'
  , readWord8, readWord16, readWord32, readWord64
  , readInt8, readInt16, readInt32, readInt64
  , readBool, readFloat, readDouble
  , readText
  , readTable
  , readPrimVector
  , readTableVector
  , readStructVector
  , readStruct
  , readStruct'
  , readStructField
  , readTableFieldOpt
  , readTableFieldReq
  , readTableFieldWithDef
  , readTableFieldUnion
  , readTableFieldUnionVectorOpt
  , readTableFieldUnionVectorReq
  ) where

import           Control.Exception.Safe        ( Exception, MonadThrow, throwM )

import           Data.Binary.Get               ( Get )
import qualified Data.Binary.Get               as G
import qualified Data.ByteString               as BS

import           Data.ByteString.Lazy          ( ByteString )
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.ByteString.Unsafe        as BSU

import           Data.Coerce                   ( coerce )
import           Data.Functor                  ( (<&>) )
import           Data.Int
import qualified Data.List                     as L
import           Data.String                   ( IsString )
import           Data.Text                     ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import           Data.Word

import           FlatBuffers.Constants
import           FlatBuffers.FileIdentifier    ( FileIdentifier(..), HasFileIdentifier(..) )
import           FlatBuffers.Internal.Positive ( Positive(getPositive), positive )
import           FlatBuffers.Types

import           HaskellWorks.Data.Int.Widen   ( widen16, widen32, widen64 )

type ReadCtx m = MonadThrow m

newtype FieldName = FieldName Text
  deriving newtype (Show, Eq, IsString)

newtype TableIndex = TableIndex { unTableIndex :: Word16 }
  deriving newtype (Show, Num)

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

-- NOTE: a uoffset should really be a Word32, but because buffers should not exceed 2^31 - 1, we use a Int32 instead.
newtype UOffset = UOffset { unUOffset :: Int32 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

-- NOTE: this is an Int32 because a buffer is assumed to respect the size limit of 2^31 - 1.
newtype OffsetFromRoot = OffsetFromRoot { unOffsetFromRoot :: Int32 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

data Table a = Table
  { vtable   :: !Position
  , tablePos :: !PositionInfo
  }

newtype Struct a = Struct
  { structPos :: Position
  }

data Union a
  = Union !a
  | UnionNone
  | UnionUnknown !Word8


type Position = ByteString

-- | Current position in the buffer
data PositionInfo = PositionInfo
  { posRoot           :: !Position        -- ^ Pointer to the buffer root
  , posCurrent        :: !Position        -- ^ Pointer to current position
  , posOffsetFromRoot :: !OffsetFromRoot  -- ^ Number of bytes between current position and root
  }

class HasPosition a where
  getPosition :: a -> Position

instance HasPosition ByteString   where getPosition = id
instance HasPosition PositionInfo where getPosition = posCurrent

decode :: forall a m. ReadCtx m => ByteString -> m (Table a)
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
      BSL.take fileIdentifierSize .
        BSL.drop uoffsetSize $
          bs


----------------------------------
------------ Vectors -------------
----------------------------------
moveToElem' :: Int32 -> Int32 -> Position -> Position
moveToElem' ix elemSize pos =
  let elemOffset = int32Size + (ix * elemSize)
  in move' pos (fromIntegral @Int32 @Int64 elemOffset)

moveToElem :: Int32 -> Int32 -> PositionInfo -> PositionInfo
moveToElem ix elemSize pos =
  let elemOffset = int32Size + (ix * elemSize)
  in move pos elemOffset

checkNegIndex :: Int32 -> Int32
checkNegIndex !n
  | n < 0     = error ("FlatBuffers.Read.index: negative index: " <> show n)
  | otherwise = n

inlineVectorToList :: ReadCtx m => Get a -> ByteString -> m [a]
inlineVectorToList get bs =
  flip runGetM bs $ do
    len <- G.getInt32le
    sequence $ L.genericReplicate len get

class VectorElement a where
  data Vector a

  vectorLength :: ReadCtx m => Vector a -> m Int32

  -- | If the index is too large, this might read garbage data, or fail with a `ReadError`.
  -- If the index is negative, an exception will be thrown.
  index :: ReadCtx m => Vector a -> Int32 -> m a

  toList :: ReadCtx m => Vector a -> m [a]

instance VectorElement Word8 where
  newtype Vector Word8 = Word8Vec Position
  vectorLength (Word8Vec pos) = readInt32 pos
  index (Word8Vec pos) ix = byteStringSafeIndex pos (int32Size + checkNegIndex ix)
  toList vec =
    vectorLength vec <&> \len ->
      BSL.unpack $
        BSL.take (fromIntegral @Int32 @Int64 len) $
          BSL.drop int32Size
            (coerce vec)

instance VectorElement Word16 where
  newtype Vector Word16 = Word16Vec Position
  vectorLength (Word16Vec pos) = readInt32 pos
  index (Word16Vec pos) ix = readWord16 (moveToElem' (checkNegIndex ix) word16Size pos)
  toList vec = inlineVectorToList G.getWord16le (coerce vec)

instance VectorElement Word32 where
  newtype Vector Word32 = Word32Vec Position
  vectorLength (Word32Vec pos) = readInt32 pos
  index (Word32Vec pos) ix = readWord32 (moveToElem' (checkNegIndex ix) word32Size pos)
  toList vec = inlineVectorToList G.getWord32le (coerce vec)

instance VectorElement Word64 where
  newtype Vector Word64 = Word64Vec Position
  vectorLength (Word64Vec pos) = readInt32 pos
  index (Word64Vec pos) ix = readWord64 (moveToElem' (checkNegIndex ix) word64Size pos)
  toList vec = inlineVectorToList G.getWord64le (coerce vec)

instance VectorElement Int8 where
  newtype Vector Int8 = Int8Vec Position
  vectorLength (Int8Vec pos) = readInt32 pos
  index (Int8Vec pos) ix = readInt8 (moveToElem' (checkNegIndex ix) int8Size pos)
  toList vec = inlineVectorToList G.getInt8 (coerce vec)

instance VectorElement Int16 where
  newtype Vector Int16 = Int16Vec Position
  vectorLength (Int16Vec pos) = readInt32 pos
  index (Int16Vec pos) ix = readInt16 (moveToElem' (checkNegIndex ix) int16Size pos)
  toList vec = inlineVectorToList G.getInt16le (coerce vec)

instance VectorElement Int32 where
  newtype Vector Int32 = Int32Vec Position
  vectorLength (Int32Vec pos) = readInt32 pos
  index (Int32Vec pos) ix = readInt32 (moveToElem' (checkNegIndex ix) int32Size pos)
  toList vec = inlineVectorToList G.getInt32le (coerce vec)

instance VectorElement Int64 where
  newtype Vector Int64 = Int64Vec Position
  vectorLength (Int64Vec pos) = readInt32 pos
  index (Int64Vec pos) ix = readInt64 (moveToElem' (checkNegIndex ix) int64Size pos)
  toList vec = inlineVectorToList G.getInt64le (coerce vec)

instance VectorElement Float where
  newtype Vector Float = FloatVec Position
  vectorLength (FloatVec pos) = readInt32 pos
  index (FloatVec pos) ix = readFloat (moveToElem' (checkNegIndex ix) floatSize pos)
  toList vec = inlineVectorToList G.getFloatle (coerce vec)

instance VectorElement Double where
  newtype Vector Double = DoubleVec Position
  vectorLength (DoubleVec pos) = readInt32 pos
  index (DoubleVec pos) ix = readDouble (moveToElem' (checkNegIndex ix) doubleSize pos)
  toList vec = inlineVectorToList G.getDoublele (coerce vec)

instance VectorElement Bool where
  newtype Vector Bool = BoolVec Position
  vectorLength (BoolVec pos) = readInt32 pos
  index (BoolVec pos) ix = readBool (moveToElem' (checkNegIndex ix) boolSize pos)
  toList vec = inlineVectorToList (word8ToBool <$> G.getWord8) (coerce vec)

instance VectorElement Text where
  newtype Vector Text = TextVec Position
  vectorLength (TextVec pos) = readInt32 pos
  index (TextVec pos) ix = readText (moveToElem' (checkNegIndex ix) textRefSize pos)
  toList vec = do
    len <- vectorLength vec
    go len (coerce vec)
    where
      go :: ReadCtx m => Int32 -> Position -> m [Text]
      go 0 _ = pure []
      go !len !pos = do
        let pos' = move' pos textRefSize
        head <- readText pos'
        tail <- go (len - 1) pos'
        pure $! head : tail

instance VectorElement (Struct a) where
  data Vector (Struct a) = StructVec
    { structVecPos        :: !Position
    , structVecStructSize :: !InlineSize
    }
  vectorLength = readInt32 . structVecPos
  index vec ix =
    let elemSize = fromIntegral @InlineSize @Int32 (structVecStructSize vec)
    in readStruct' (moveToElem' (checkNegIndex ix) elemSize (structVecPos vec))
  toList vec = do
    len <- vectorLength vec
    if len == 0
      then pure []
      else pure $ go len (move' (structVecPos vec) int32Size)
    where
      go :: Int32 -> Position -> [Struct a]
      go !len !pos =
        let head = readStruct pos
            tail =
              if len == 1
                then []
                else go (len - 1) (move' pos (fromIntegral @InlineSize @Int64 (structVecStructSize vec)))
        in  head : tail

instance VectorElement (Table a) where
  newtype Vector (Table a) = TableVec PositionInfo
  vectorLength (TableVec pos) = readInt32 pos
  index vec ix = readTable (moveToElem (checkNegIndex ix) tableRefSize (coerce vec))
  toList vec = do
    len <- vectorLength vec
    go len (coerce vec)
    where
      go :: ReadCtx m => Int32 -> PositionInfo -> m [Table a]
      go 0 _ = pure []
      go !len !pos = do
        let pos' = move pos tableRefSize
        head <- readTable pos'
        tail <- go (len - 1) pos'
        pure $! head : tail

instance VectorElement (Union a) where
  data Vector (Union a) = UnionVec
    { unionVecTypesPos  :: !(Vector Word8)
    -- ^ A byte-vector, where each byte represents the type of each "union value" in the vector
    , unionVecValuesPos :: !PositionInfo
    -- ^ A table vector, with the actual union values
    , unionVecElemRead  :: !(forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union a))
    -- ^ A function to read a union value from this vector
    }
  -- NOTE: we assume the two vectors have the same length
  vectorLength = readInt32 . unionVecValuesPos

  index vec ix = do
    unionType <- index (unionVecTypesPos vec) ix
    case positive unionType of
      Nothing         -> pure UnionNone
      Just unionType' ->
        let readElem = (unionVecElemRead vec) unionType'
        in  readElem (moveToElem ix tableRefSize (unionVecValuesPos vec))

  toList vec = do
    len <- vectorLength vec
    if len == 0
      then pure []
      else go
            len
            (move' (coerce unionVecTypesPos vec) 4)
            (move (unionVecValuesPos vec) 4)
    where
      go :: ReadCtx m => Int32 -> Position -> PositionInfo -> m [Union a]
      go !len !valuesPos !typesPos = do
        unionType <- readWord8 valuesPos
        head <- case positive unionType of
                  Nothing -> pure UnionNone
                  Just unionType' ->
                    let readElem = (unionVecElemRead vec) unionType'
                    in  readElem typesPos
        tail <- if len == 1
                  then pure []
                  else go (len - 1) (BSL.drop 1 valuesPos) (move typesPos 4)
        pure $! head : tail

----------------------------------
----- Read from Struct/Table -----
----------------------------------
readStructField :: (Position -> a) -> VOffset -> Struct s -> a
readStructField read voffset (Struct bs) =
  read (move' bs (fromIntegral @VOffset @Int64 voffset))

readTableFieldOpt :: ReadCtx m => (PositionInfo -> m a) -> TableIndex -> Table t -> m (Maybe a)
readTableFieldOpt read ix t = do
  mbOffset <- tableIndexToVOffset t ix
  traverse (\offset -> read (moveV (tablePos t) offset)) mbOffset

readTableFieldReq :: ReadCtx m => (PositionInfo -> m a) -> TableIndex -> FieldName -> Table t -> m a
readTableFieldReq read ix name t = do
  mbOffset <- tableIndexToVOffset t ix
  case mbOffset of
    Nothing -> throwM $ MissingField name
    Just offset -> read (moveV (tablePos t) offset)

readTableFieldWithDef :: ReadCtx m => (PositionInfo -> m a) -> TableIndex -> a -> Table t -> m a
readTableFieldWithDef read ix dflt t =
  tableIndexToVOffset t ix >>= \case
    Nothing -> pure dflt
    Just offset -> read (moveV (tablePos t) offset)

readTableFieldUnion :: ReadCtx m => (Positive Word8 -> PositionInfo -> m (Union a)) -> TableIndex -> Table t -> m (Union a)
readTableFieldUnion read ix t =
  readTableFieldWithDef readWord8 ix 0 t >>= \unionType ->
    case positive unionType of
      Nothing         -> pure UnionNone
      Just unionType' ->
        tableIndexToVOffset t (ix + 1) >>= \case
          Nothing     -> throwM $ MalformedBuffer "Union: 'union type' found but 'union value' is missing."
          Just offset -> read unionType' (moveV (tablePos t) offset)

readTableFieldUnionVectorOpt :: ReadCtx m
  => (forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union a))
  -> TableIndex
  -> Table t
  -> m (Maybe (Vector (Union a)))
readTableFieldUnionVectorOpt read ix t =
  tableIndexToVOffset t ix >>= \case
    Nothing -> pure Nothing
    Just typesOffset ->
      tableIndexToVOffset t (ix + 1) >>= \case
        Nothing -> throwM $ MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          Just <$> readUnionVector read (moveV (tablePos t) typesOffset) (moveV (tablePos t) valuesOffset)

readTableFieldUnionVectorReq :: ReadCtx m
  => (forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union a))
  -> TableIndex
  -> FieldName
  -> Table t
  -> m (Vector (Union a))
readTableFieldUnionVectorReq read ix name t =
  tableIndexToVOffset t ix >>= \case
    Nothing -> throwM $ MissingField name
    Just typesOffset ->
      tableIndexToVOffset t (ix + 1) >>= \case
        Nothing -> throwM $ MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          readUnionVector read (moveV (tablePos t) typesOffset) (moveV (tablePos t) valuesOffset)

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
readBool p = word8ToBool <$> readWord8 p

word8ToBool :: Word8 -> Bool
word8ToBool 0 = False
word8ToBool _ = True

readPrimVector ::
     forall a m. ReadCtx m
  => (Position -> Vector a)
  -> PositionInfo
  -> m (Vector a)
readPrimVector vecConstructor (posCurrent -> pos) = do
  uoffset <- readInt32 pos
  pure $! vecConstructor
    (move' pos (fromIntegral @Int32 @Int64 uoffset))

readTableVector ::
     forall a m. ReadCtx m
  => PositionInfo
  -> m (Vector (Table a))
readTableVector pos = do
  uoffset <- readInt32 pos
  pure $! TableVec
    (move pos (coerce uoffset))

readStructVector ::
     forall a m. ReadCtx m
  => InlineSize
  -> PositionInfo
  -> m (Vector (Struct a))
readStructVector structSize (posCurrent -> pos) = do
  uoffset <- readInt32 pos
  pure $! StructVec
    (move' pos (fromIntegral @Int32 @Int64 uoffset))
    structSize

readUnionVector ::
     forall a m. ReadCtx m
  => (forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union a))
  -> PositionInfo
  -> PositionInfo
  -> m (Vector (Union a))
readUnionVector readUnion typesPos valuesPos =
  do
    typesVec <- readPrimVector Word8Vec typesPos
    valuesVecUOffset <- readInt32 valuesPos
    pure $! UnionVec
      typesVec
      (move valuesPos valuesVecUOffset)
      readUnion

readText :: (ReadCtx m, HasPosition a) => a -> m Text
readText (getPosition -> pos) = do
  bs <- flip runGetM pos $ do
    readAndSkipUOffset
    strLength <- G.getInt32le
    -- NOTE: this might overflow in systems where Int has less than 32 bytes
    G.getByteString $ fromIntegral @Int32 @Int strLength
  case T.decodeUtf8' bs of
    Right t -> pure t
    Left (T.DecodeError msg b) -> throwM $ Utf8DecodingError (T.pack msg) b
    -- The `EncodeError` constructor is deprecated and not used
    -- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Encoding-Error.html#t:UnicodeException
    Left _ -> error "the impossible happened"

-- | Convenience function for reading structs from table fields / vectors
readStruct' :: (Applicative f, HasPosition a) => a -> f (Struct s)
readStruct' = pure . readStruct

readStruct :: HasPosition a => a -> Struct s
readStruct (getPosition -> pos) = Struct pos

readTable :: forall t m. ReadCtx m => PositionInfo -> m (Table t)
readTable pos@PositionInfo{..} =
  flip runGetM posCurrent $ do
    tableOffset <- readAndSkipUOffset
    soffset <- G.getInt32le

    let vtableOffsetFromRoot = coerce posOffsetFromRoot + coerce tableOffset - widen32 soffset
    let vtable = move' posRoot (fromIntegral @Int32 @Int64 vtableOffsetFromRoot)
    pure $ Table vtable (move pos (coerce tableOffset))


----------------------------------
---------- Primitives ------------
----------------------------------
tableIndexToVOffset :: ReadCtx m => Table t -> TableIndex -> m (Maybe VOffset)
tableIndexToVOffset Table{..} ix =
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
moveV pos offset = move pos (fromIntegral @VOffset @Int32 offset)

move :: PositionInfo -> Int32 -> PositionInfo
move PositionInfo{..} offset =
  PositionInfo
  { posRoot = posRoot
  , posCurrent = move' posCurrent (fromIntegral @Int32 @Int64 offset)
  , posOffsetFromRoot = posOffsetFromRoot + OffsetFromRoot offset
  }

move' :: Position -> Int64 -> ByteString
move' bs offset = BSL.drop offset bs

readAndSkipUOffset :: Get UOffset
readAndSkipUOffset = do
  uoffset <- G.getInt32le
  -- NOTE: this might overflow in systems where Int has less than 32 bytes
  G.skip (fromIntegral @Int32 @Int (uoffset - uoffsetSize))
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
byteStringSafeIndex :: ReadCtx m => ByteString -> Int32 -> m Word8
byteStringSafeIndex !cs0 !i =
  index' cs0 i
  where index' BSL.Empty _ = throwM $ MalformedBuffer "Buffer has fewer bytes than indicated by the vector length"
        index' (BSL.Chunk c cs) n
          -- NOTE: this might overflow in systems where Int has less than 32 bytes
          | fromIntegral @Int32 @Int n >= BS.length c =
              -- Note: it's safe to narrow `BS.length` to an int32 here, the line above proves it.
              index' cs (n - fromIntegral @Int @Int32 (BS.length c))
          | otherwise = pure $! BSU.unsafeIndex c (fromIntegral @Int32 @Int n)
