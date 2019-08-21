{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module FlatBuffers.Internal.Read
  ( TableIndex(..)
  , VOffset(..)
  , ReadError(..)
  , Struct(..)
  , Table(..)
  , HasPosition(..)
  , Position
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
  , readTable'
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

import           Control.DeepSeq               ( NFData )
import           Control.Exception             ( Exception )
import           Control.Monad                 ( join, (>=>) )

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
import           Data.Text                     ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import           Data.Word

import           FlatBuffers.Constants
import           FlatBuffers.FileIdentifier    ( FileIdentifier(..), HasFileIdentifier(..) )
import           FlatBuffers.Internal.Util     ( Positive, positive )
import           FlatBuffers.Types

import           GHC.Generics                  ( Generic )

newtype TableIndex = TableIndex { unTableIndex :: Word16 }
  deriving newtype (Show, Num)

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

-- NOTE: this is an Int32 because a buffer is assumed to respect the size limit of 2^31 - 1.
newtype OffsetFromRoot = OffsetFromRoot Int32
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
  move :: Integral i => a -> i -> a

instance HasPosition ByteString where
  getPosition = id
  move bs offset = BSL.drop (fromIntegral @_ @Int64 offset) bs

instance HasPosition PositionInfo where
  getPosition = posCurrent
  move PositionInfo{..} offset =
    PositionInfo
    { posRoot = posRoot
    , posCurrent = move posCurrent offset
    , posOffsetFromRoot = posOffsetFromRoot + OffsetFromRoot (fromIntegral @_ @Int32 offset)
    }

decode :: ByteString -> Either ReadError (Table a)
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
{-# INLINE moveToElem #-}
moveToElem :: HasPosition pos => pos -> Int32 -> Int32 -> pos
moveToElem pos elemSize ix =
  let elemOffset = int32Size + (ix * elemSize)
  in move pos elemOffset

{-# INLINE checkNegIndex #-}
checkNegIndex :: Int32 -> Int32
checkNegIndex !n
  | n < 0     = error ("FlatBuffers.Read.index: negative index: " <> show n)
  | otherwise = n

{-# INLINE inlineVectorToList #-}
inlineVectorToList :: Get a -> ByteString -> Either ReadError [a]
inlineVectorToList get bs =
  flip runGet bs $ do
    len <- G.getInt32le
    sequence $ L.replicate (fromIntegral @Int32 @Int len) get

class VectorElement a where
  data Vector a

  vectorLength :: Vector a -> Either ReadError Int32

  -- | If the index is too large, this might read garbage data, or fail with a `ReadError`.
  -- If the index is negative, an exception will be thrown.
  index :: Vector a -> Int32 -> Either ReadError a

  toList :: Vector a -> Either ReadError [a]

instance VectorElement Word8 where
  newtype Vector Word8 = VectorWord8 Position
  vectorLength (VectorWord8 pos) = readInt32 pos
  index (VectorWord8 pos) ix = byteStringSafeIndex pos (int32Size + checkNegIndex ix)
  toList vec =
    vectorLength vec <&> \len ->
      BSL.unpack $
        BSL.take (fromIntegral @Int32 @Int64 len) $
          BSL.drop int32Size
            (coerce vec)

instance VectorElement Word16 where
  newtype Vector Word16 = VectorWord16 Position
  vectorLength (VectorWord16 pos) = readInt32 pos
  index (VectorWord16 pos) = readWord16 . moveToElem pos word16Size . checkNegIndex
  toList vec = inlineVectorToList G.getWord16le (coerce vec)

instance VectorElement Word32 where
  newtype Vector Word32 = VectorWord32 Position
  vectorLength (VectorWord32 pos) = readInt32 pos
  index (VectorWord32 pos) = readWord32 . moveToElem pos word32Size . checkNegIndex
  toList vec = inlineVectorToList G.getWord32le (coerce vec)

instance VectorElement Word64 where
  newtype Vector Word64 = VectorWord64 Position
  vectorLength (VectorWord64 pos) = readInt32 pos
  index (VectorWord64 pos) = readWord64 . moveToElem pos word64Size . checkNegIndex
  toList vec = inlineVectorToList G.getWord64le (coerce vec)

instance VectorElement Int8 where
  newtype Vector Int8 = VectorInt8 Position
  vectorLength (VectorInt8 pos) = readInt32 pos
  index (VectorInt8 pos) = readInt8 . moveToElem pos int8Size . checkNegIndex
  toList vec = inlineVectorToList G.getInt8 (coerce vec)

instance VectorElement Int16 where
  newtype Vector Int16 = VectorInt16 Position
  vectorLength (VectorInt16 pos) = readInt32 pos
  index (VectorInt16 pos) = readInt16 . moveToElem pos int16Size . checkNegIndex
  toList vec = inlineVectorToList G.getInt16le (coerce vec)

instance VectorElement Int32 where
  newtype Vector Int32 = VectorInt32 Position
  vectorLength (VectorInt32 pos) = readInt32 pos
  index (VectorInt32 pos) = readInt32 . moveToElem pos int32Size . checkNegIndex
  toList vec = inlineVectorToList G.getInt32le (coerce vec)

instance VectorElement Int64 where
  newtype Vector Int64 = VectorInt64 Position
  vectorLength (VectorInt64 pos) = readInt32 pos
  index (VectorInt64 pos) = readInt64 . moveToElem pos int64Size . checkNegIndex
  toList vec = inlineVectorToList G.getInt64le (coerce vec)

instance VectorElement Float where
  newtype Vector Float = VectorFloat Position
  vectorLength (VectorFloat pos) = readInt32 pos
  index (VectorFloat pos) = readFloat . moveToElem pos floatSize . checkNegIndex
  toList vec = inlineVectorToList G.getFloatle (coerce vec)

instance VectorElement Double where
  newtype Vector Double = VectorDouble Position
  vectorLength (VectorDouble pos) = readInt32 pos
  index (VectorDouble pos) = readDouble . moveToElem pos doubleSize . checkNegIndex
  toList vec = inlineVectorToList G.getDoublele (coerce vec)

instance VectorElement Bool where
  newtype Vector Bool = VectorBool Position
  vectorLength (VectorBool pos) = readInt32 pos
  index (VectorBool pos) = readBool . moveToElem pos boolSize . checkNegIndex
  toList (VectorBool pos) = fmap word8ToBool <$> toList (VectorWord8 pos)

instance VectorElement Text where
  newtype Vector Text = VectorText Position
  vectorLength (VectorText pos) = readInt32 pos
  index (VectorText pos) = readText . moveToElem pos textRefSize . checkNegIndex

  toList :: Vector Text -> Either ReadError [Text]
  toList (VectorText pos) = do
    positions <- toList (VectorInt32 pos)
    L.reverse <$> go positions 0 []
    where
      go :: [Int32] -> Int32 -> [Text] -> Either ReadError [Text]
      go [] _ acc = Right acc
      go (offset : xs) ix acc = do
        let textPos = move pos (offset + (ix * 4) + 4)
        text <- join $ runGet readText' textPos
        go xs (ix + 1) (text : acc)

instance VectorElement (Struct a) where
  data Vector (Struct a) = VectorStruct
    { vectorStructStructSize :: !InlineSize
    , vectorStructPos        :: !Position
    }
  vectorLength = readInt32 . vectorStructPos
  index (VectorStruct structSize pos) ix =
    let elemSize = fromIntegral @InlineSize @Int32 structSize
    in readStruct' . moveToElem pos elemSize . checkNegIndex $ ix
  toList vec@(VectorStruct structSize pos) =
    vectorLength vec <&> \len ->
      go len (move pos int32Size)
    where
      go :: Int32 -> Position -> [Struct a]
      go 0 _ = []
      go !len pos =
        let head = readStruct pos
            tail = go (len - 1) (move pos structSize)
        in  head : tail

instance VectorElement (Table a) where
  newtype Vector (Table a) = VectorTable PositionInfo
  vectorLength (VectorTable pos) = readInt32 pos
  index vec = readTable . moveToElem (coerce vec) tableRefSize . checkNegIndex
  toList (VectorTable vectorPos) = do
    positions <- toList (VectorInt32 (posCurrent vectorPos))
    go positions 0
    where
      go :: [Int32] -> Int32 -> Either ReadError [Table a]
      go [] _ = Right []
      go (offset : offsets) !ix = do
        let tablePos = move vectorPos (offset + (ix * 4) + 4)
        table <- readTable' tablePos
        tables <- go offsets (ix + 1)
        pure (table : tables)

instance VectorElement (Union a) where
  data Vector (Union a) = VectorUnion
    { vectorUnionTypesPos  :: !(Vector Word8)
    -- ^ A byte-vector, where each byte represents the type of each "union value" in the vector
    , vectorUnionValuesPos :: !PositionInfo
    -- ^ A table vector, with the actual union values
    , vectorUnionReadElem  :: !(Positive Word8 -> PositionInfo -> Either ReadError (Union a))
    -- ^ A function to read a union value from this vector
    }
  -- NOTE: we assume the two vectors have the same length
  vectorLength = readInt32 . vectorUnionValuesPos

  index (VectorUnion typesPos valuesPos readElem) ix = do
    unionType <- index typesPos ix
    case positive unionType of
      Nothing         -> Right UnionNone
      Just unionType' ->
        let tablePos = moveToElem valuesPos tableRefSize ix
        in  readElem unionType' tablePos

  toList vec@(VectorUnion typesPos valuesPos readElem) = do
    len <- vectorLength vec
    if len == 0
      then Right []
      else go
            len
            (move (coerce typesPos) 4)
            (move valuesPos 4)
    where
      go :: Int32 -> Position -> PositionInfo -> Either ReadError [Union a]
      go !len !valuesPos !typesPos = do
        unionType <- readWord8 valuesPos
        head <- case positive unionType of
                  Nothing -> Right UnionNone
                  Just unionType' -> readElem unionType' typesPos
        tail <- if len == 1
                  then Right []
                  else go (len - 1) (BSL.drop 1 valuesPos) (move typesPos 4)
        Right $! head : tail

----------------------------------
----- Read from Struct/Table -----
----------------------------------
readStructField :: (Position -> a) -> VOffset -> Struct s -> a
readStructField read voffset (Struct bs) =
  read (move bs voffset)

readTableFieldOpt :: (PositionInfo -> Either ReadError a) -> TableIndex -> Table t -> Either ReadError (Maybe a)
readTableFieldOpt read ix t = do
  mbOffset <- tableIndexToVOffset t ix
  traverse (\offset -> read (move (tablePos t) offset)) mbOffset

readTableFieldReq :: (PositionInfo -> Either ReadError a) -> TableIndex -> Text -> Table t -> Either ReadError a
readTableFieldReq read ix name t = do
  mbOffset <- tableIndexToVOffset t ix
  case mbOffset of
    Nothing -> Left $ MissingField name
    Just offset -> read (move (tablePos t) offset)

readTableFieldWithDef :: (PositionInfo -> Either ReadError a) -> TableIndex -> a -> Table t -> Either ReadError a
readTableFieldWithDef read ix dflt t =
  tableIndexToVOffset t ix >>= \case
    Nothing -> Right dflt
    Just offset -> read (move (tablePos t) offset)

readTableFieldUnion :: (Positive Word8 -> PositionInfo -> Either ReadError (Union a)) -> TableIndex -> Table t -> Either ReadError (Union a)
readTableFieldUnion read ix t =
  readTableFieldWithDef readWord8 (ix - 1) 0 t >>= \unionType ->
    case positive unionType of
      Nothing         -> Right UnionNone
      Just unionType' ->
        tableIndexToVOffset t ix >>= \case
          Nothing     -> Left $ MalformedBuffer "Union: 'union type' found but 'union value' is missing."
          Just offset -> read unionType' (move (tablePos t) offset)

readTableFieldUnionVectorOpt ::
     (Positive Word8 -> PositionInfo -> Either ReadError (Union a))
  -> TableIndex
  -> Table t
  -> Either ReadError (Maybe (Vector (Union a)))
readTableFieldUnionVectorOpt read ix t =
  tableIndexToVOffset t (ix - 1) >>= \case
    Nothing -> Right Nothing
    Just typesOffset ->
      tableIndexToVOffset t ix >>= \case
        Nothing -> Left $ MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          Just <$> readUnionVector read (move (tablePos t) typesOffset) (move (tablePos t) valuesOffset)

readTableFieldUnionVectorReq ::
     (Positive Word8 -> PositionInfo -> Either ReadError (Union a))
  -> TableIndex
  -> Text
  -> Table t
  -> Either ReadError (Vector (Union a))
readTableFieldUnionVectorReq read ix name t =
  tableIndexToVOffset t (ix - 1) >>= \case
    Nothing -> Left $ MissingField name
    Just typesOffset ->
      tableIndexToVOffset t ix >>= \case
        Nothing -> Left $ MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          readUnionVector read (move (tablePos t) typesOffset) (move (tablePos t) valuesOffset)

----------------------------------
------ Read from `Position` ------
----------------------------------
readInt8 :: HasPosition a => a -> Either ReadError Int8
readInt8 (getPosition -> pos) = runGet G.getInt8 pos

readInt16 :: HasPosition a => a -> Either ReadError Int16
readInt16 (getPosition -> pos) = runGet G.getInt16le pos

readInt32 :: HasPosition a => a -> Either ReadError Int32
readInt32 (getPosition -> pos) = runGet G.getInt32le pos

readInt64 :: HasPosition a => a -> Either ReadError Int64
readInt64 (getPosition -> pos) = runGet G.getInt64le pos

readWord8 :: HasPosition a => a -> Either ReadError Word8
readWord8 (getPosition -> pos) = runGet G.getWord8 pos

readWord16 :: HasPosition a => a -> Either ReadError Word16
readWord16 (getPosition -> pos) = runGet G.getWord16le pos

readWord32 :: HasPosition a => a -> Either ReadError Word32
readWord32 (getPosition -> pos) = runGet G.getWord32le pos

readWord64 :: HasPosition a => a -> Either ReadError Word64
readWord64 (getPosition -> pos) = runGet G.getWord64le pos

readFloat :: HasPosition a => a -> Either ReadError Float
readFloat (getPosition -> pos) = runGet G.getFloatle pos

readDouble :: HasPosition a => a -> Either ReadError Double
readDouble (getPosition -> pos) = runGet G.getDoublele pos

readBool :: HasPosition a => a -> Either ReadError Bool
readBool p = word8ToBool <$> readWord8 p

{-# INLINE word8ToBool #-}
word8ToBool :: Word8 -> Bool
word8ToBool 0 = False
word8ToBool _ = True

readPrimVector ::
     (Position -> Vector a)
  -> PositionInfo
  -> Either ReadError (Vector a)
readPrimVector vecConstructor (posCurrent -> pos) =
  vecConstructor <$> readUOffsetAndSkip pos

readTableVector :: PositionInfo -> Either ReadError (Vector (Table a))
readTableVector pos =
  VectorTable <$> readUOffsetAndSkip pos

readStructVector :: forall a. IsStruct a => PositionInfo -> Either ReadError (Vector (Struct a))
readStructVector (posCurrent -> pos) =
  VectorStruct (structSizeOf @a) <$> readUOffsetAndSkip pos

readUnionVector ::
     (Positive Word8 -> PositionInfo -> Either ReadError (Union a))
  -> PositionInfo
  -> PositionInfo
  -> Either ReadError (Vector (Union a))
readUnionVector readUnion typesPos valuesPos =
  do
    typesVec <- readPrimVector VectorWord8 typesPos
    valuesVec <- readUOffsetAndSkip valuesPos
    Right $! VectorUnion
      typesVec
      valuesVec
      readUnion

-- | Follow a pointer to the position of a string and read it.
readText :: HasPosition a => a -> Either ReadError Text
readText (getPosition -> pos) =
  join $ flip runGet pos $ do
    uoffset <- G.getInt32le
    -- NOTE: this might overflow in systems where Int has less than 32 bits
    G.skip (fromIntegral @Int32 @Int (uoffset - uoffsetSize))
    readText'

-- | Read a string from the current buffer position.
{-# INLINE readText' #-}
readText' :: Get (Either ReadError Text)
readText' = do
  strLength <- G.getInt32le
  -- NOTE: this might overflow in systems where Int has less than 32 bits
  bs <- G.getByteString $ fromIntegral @Int32 @Int strLength
  pure $! case T.decodeUtf8' bs of
    Right t -> Right t
    Left (T.DecodeError msg b) -> Left $ Utf8DecodingError (T.pack msg) b
    -- The `EncodeError` constructor is deprecated and not used
    -- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Encoding-Error.html#t:UnicodeException
    Left _ -> error "the impossible happened"

-- | Follow a pointer to the position of a table and read it.
readTable :: PositionInfo -> Either ReadError (Table t)
readTable = readUOffsetAndSkip >=> readTable'

-- | Read a table from the current buffer position.
{-# INLINE readTable' #-}
readTable' :: PositionInfo -> Either ReadError (Table t)
readTable' tablePos =
  readInt32 tablePos <&> \soffset ->
    let vtableOffsetFromRoot = coerce (posOffsetFromRoot tablePos) - soffset
        vtable = move (posRoot tablePos) vtableOffsetFromRoot
    in  Table vtable tablePos

-- | Convenience function for reading structs from table fields / vectors
readStruct' :: HasPosition a => a -> Either ReadError (Struct s)
readStruct' = Right . readStruct

readStruct :: HasPosition a => a -> Struct s
readStruct = Struct . getPosition

----------------------------------
---------- Primitives ------------
----------------------------------
tableIndexToVOffset :: Table t -> TableIndex -> Either ReadError (Maybe VOffset)
tableIndexToVOffset Table{..} ix =
  flip runGet vtable $ do
    vtableSize <- G.getWord16le
    let vtableIndex = 4 + (unTableIndex ix * 2)
    if vtableIndex >= vtableSize
      then pure Nothing
      else do
        G.skip (fromIntegral @Word16 @Int vtableIndex - 2)
        G.getWord16le <&> \case
          0 -> Nothing
          word16 -> Just (VOffset word16)

{-# INLINE readUOffsetAndSkip #-}
readUOffsetAndSkip :: HasPosition pos => pos -> Either ReadError pos
readUOffsetAndSkip pos =
  move pos <$> readInt32 pos

data ReadError
  = ParsingError { position :: !G.ByteOffset
                 , msg      :: !Text }
  | MissingField { fieldName :: !Text }
  | Utf8DecodingError { msg  :: !Text
                      , byte :: !(Maybe Word8) }
  | MalformedBuffer !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Exception)

{-# INLINE runGet #-}
runGet :: Get a -> ByteString -> Either ReadError a
runGet get bs =
  case G.runGetOrFail get bs of
    Right (_, _, a) -> Right a
    Left (_, pos, msg) -> Left $ ParsingError pos (T.pack msg)

-- Adapted from `Data.ByteString.Lazy.index`: https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.Lazy.html#index
-- Assumes i >= 0.
{-# INLINE byteStringSafeIndex #-}
byteStringSafeIndex :: ByteString -> Int32 -> Either ReadError Word8
byteStringSafeIndex !cs0 !i =
  index' cs0 i
  where index' BSL.Empty _ = Left $ MalformedBuffer "Buffer has fewer bytes than indicated by the vector length"
        index' (BSL.Chunk c cs) n
          -- NOTE: this might overflow in systems where Int has less than 32 bits
          | fromIntegral @Int32 @Int n >= BS.length c =
              -- Note: it's safe to narrow `BS.length` to an int32 here, the line above proves it.
              index' cs (n - fromIntegral @Int @Int32 (BS.length c))
          | otherwise = Right $! BSU.unsafeIndex c (fromIntegral @Int32 @Int n)
