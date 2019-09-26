{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_HADDOCK not-home #-}

{- HLINT ignore readTableFieldOpt "Avoid lambda" -}

-- Using `replicateM` here causes a performance regression.
{- HLINT ignore inlineVectorToList "Use replicateM" -}

module FlatBuffers.Internal.Read where

import           Control.Monad                       ( (>=>), join )

import           Data.Binary.Get                     ( Get )
import qualified Data.Binary.Get                     as G
import qualified Data.ByteString                     as BS
import           Data.ByteString.Lazy                ( ByteString )
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Internal       as BSL
import qualified Data.ByteString.Unsafe              as BSU
import           Data.Coerce                         ( coerce )
import           Data.Functor                        ( (<&>) )
import           Data.Int
import qualified Data.List                           as L
import           Data.Text                           ( Text )
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.Encoding.Error            as T
import           Data.Word

import           FlatBuffers.Internal.Constants
import           FlatBuffers.Internal.FileIdentifier ( FileIdentifier(..), HasFileIdentifier(..) )
import           FlatBuffers.Internal.Types
import           FlatBuffers.Internal.Util           ( Positive, positive )

import           Prelude                             hiding ( drop, length, take )

type ReadError = String

newtype TableIndex = TableIndex { unTableIndex :: Word16 }
  deriving newtype (Show, Num)

newtype VOffset = VOffset { unVOffset :: Word16 }
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

-- NOTE: this is an Int32 because a buffer is assumed to respect the size limit of 2^31 - 1.
newtype OffsetFromRoot = OffsetFromRoot Int32
  deriving newtype (Show, Num, Real, Ord, Enum, Integral, Eq)

-- | A table that is being read from a flatbuffer.
data Table a = Table
  { vtable   :: !Position
  , tablePos :: !PositionInfo
  }

-- | A struct that is being read from a flatbuffer.
newtype Struct a = Struct
  { structPos :: Position
  }

-- | A union that is being read from a flatbuffer.
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

-- | Deserializes a flatbuffer from a lazy `ByteString`.
decode :: ByteString -> Either ReadError (Table a)
decode root = readTable initialPos
  where
    initialPos = PositionInfo root root 0

-- | Checks if a buffer contains the file identifier for a root table @a@, to see if it's
-- safe to decode it to a `Table`.
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
  move pos (ix * elemSize)

{-# INLINE checkIndexBounds #-}
checkIndexBounds :: Int32 -> Int32 -> Int32
checkIndexBounds ix length
  | ix < 0       = error ("FlatBuffers.Internal.Read.index: negative index: " <> show ix)
  | ix >= length = error ("FlatBuffers.Internal.Read.index: index too large: " <> show ix)
  | otherwise    = ix

{-# INLINE inlineVectorToList #-}
inlineVectorToList :: Get a -> Int32 -> Position -> Either ReadError [a]
inlineVectorToList get len pos =
  runGet pos $
    sequence $ L.replicate (fromIntegral @Int32 @Int len) get

-- | @clamp n upperBound@ truncates a value to stay between @0@ and @upperBound@.
clamp :: Int32 -> Int32 -> Int32
clamp n upperBound = n `min` upperBound `max` 0

class VectorElement a where

  -- | A vector that is being read from a flatbuffer.
  data Vector a

  -- | Returns the size of the vector.
  --
  -- /O(1)/.
  length :: Vector a -> Int32

  -- | Returns the item at the given index without performing the bounds check.
  --
  -- Given an invalid index, @unsafeIndex@ will likely read garbage data or return a `ReadError`.
  -- In the case of @Vector Word8@, using a negative index carries the same risks as `BSU.unsafeIndex`
  -- (i.e. reading from outside the buffer's  boundaries).
  --
  -- /O(c)/, where /c/ is the number of chunks in the underlying `ByteString`.
  unsafeIndex :: Vector a -> Int32 -> Either ReadError a

  -- | Converts the vector to a list.
  --
  -- /O(n)/.
  toList :: Vector a -> Either ReadError [a]

  -- | @take n xs@ returns the prefix of @xs@ of length @n@, or @xs@ itself if @n > length xs@.
  --
  -- /O(1)/.
  take :: Int32 -> Vector a -> Vector a

  -- | @drop n xs@ returns the suffix of @xs@ after the first @n@ elements, or @[]@ if @n > length xs@.
  --
  -- /O(c)/, where /c/ is the number of chunks in the underlying `ByteString`.
  drop :: Int32 -> Vector a -> Vector a

-- | Returns the item at the given index.
-- If the given index is negative or too large, an `error` is thrown.
--
-- /O(c)/, where /c/ is the number of chunks in the underlying `ByteString`.
index :: VectorElement a => Vector a -> Int32 -> Either ReadError a
index vec ix = unsafeIndex vec . checkIndexBounds ix $ length vec


instance VectorElement Word8 where
  data Vector Word8 = VectorWord8 !Int32 !Position

  length (VectorWord8 len _)         = len
  unsafeIndex (VectorWord8 _ pos) ix = byteStringSafeIndex pos ix
  take n (VectorWord8 len pos)       = VectorWord8 (clamp n len) pos
  drop n (VectorWord8 len pos)       = VectorWord8 (clamp (len - n) len) (BSL.drop (fromIntegral @Int32 @Int64 n) pos)

  toList (VectorWord8 len pos) =
    Right $
      BSL.unpack $
        BSL.take (fromIntegral @Int32 @Int64 len) $
          pos

instance VectorElement Word16 where
  data Vector Word16 = VectorWord16 !Int32 !Position

  length (VectorWord16 len _)      = len
  unsafeIndex (VectorWord16 _ pos) = readWord16 . moveToElem pos word16Size
  take n (VectorWord16 len pos)    = VectorWord16 (clamp n len) pos
  drop n (VectorWord16 len pos)    = VectorWord16 (len - n') (moveToElem pos word16Size n')
    where n' = clamp n len
  toList (VectorWord16 len pos)    = inlineVectorToList G.getWord16le len pos

instance VectorElement Word32 where
  data Vector Word32 = VectorWord32 !Int32 !Position

  length (VectorWord32 len _)      = len
  unsafeIndex (VectorWord32 _ pos) = readWord32 . moveToElem pos word32Size
  take n (VectorWord32 len pos)    = VectorWord32 (clamp n len) pos
  drop n (VectorWord32 len pos)    = VectorWord32 (len - n') (moveToElem pos word32Size n')
    where n' = clamp n len
  toList (VectorWord32 len pos)    = inlineVectorToList G.getWord32le len pos

instance VectorElement Word64 where
  data Vector Word64 = VectorWord64 !Int32 !Position

  length (VectorWord64 len _)      = len
  unsafeIndex (VectorWord64 _ pos) = readWord64 . moveToElem pos word64Size
  take n (VectorWord64 len pos)    = VectorWord64 (clamp n len) pos
  drop n (VectorWord64 len pos)    = VectorWord64 (len - n') (moveToElem pos word64Size n')
    where n' = clamp n len
  toList (VectorWord64 len pos)    = inlineVectorToList G.getWord64le len pos

instance VectorElement Int8 where
  data Vector Int8 = VectorInt8 !Int32 !Position

  length (VectorInt8 len _)        = len
  unsafeIndex (VectorInt8 _ pos)   = readInt8 . moveToElem pos int8Size
  take n (VectorInt8 len pos)      = VectorInt8 (clamp n len) pos
  drop n (VectorInt8 len pos)      = VectorInt8 (len - n') (moveToElem pos int8Size n')
    where n' = clamp n len
  toList (VectorInt8 len pos)      = inlineVectorToList G.getInt8 len pos

instance VectorElement Int16 where
  data Vector Int16 = VectorInt16 !Int32 !Position

  length (VectorInt16 len _)       = len
  unsafeIndex (VectorInt16 _ pos)  = readInt16 . moveToElem pos int16Size
  take n (VectorInt16 len pos)     = VectorInt16 (clamp n len) pos
  drop n (VectorInt16 len pos)     = VectorInt16 (len - n') (moveToElem pos int16Size n')
    where n' = clamp n len
  toList (VectorInt16 len pos)     = inlineVectorToList G.getInt16le len pos

instance VectorElement Int32 where
  data Vector Int32 = VectorInt32 !Int32 !Position

  length (VectorInt32 len _)       = len
  unsafeIndex (VectorInt32 _ pos)  = readInt32 . moveToElem pos int32Size
  take n (VectorInt32 len pos)     = VectorInt32 (clamp n len) pos
  drop n (VectorInt32 len pos)     = VectorInt32 (len - n') (moveToElem pos int32Size n')
    where n' = clamp n len
  toList (VectorInt32 len pos)     = inlineVectorToList G.getInt32le len pos

instance VectorElement Int64 where
  data Vector Int64 = VectorInt64 !Int32 !Position

  length (VectorInt64 len _)       = len
  unsafeIndex (VectorInt64 _ pos)  = readInt64 . moveToElem pos int64Size
  take n (VectorInt64 len pos)     = VectorInt64 (clamp n len) pos
  drop n (VectorInt64 len pos)     = VectorInt64 (len - n') (moveToElem pos int64Size n')
    where n' = clamp n len
  toList (VectorInt64 len pos)     = inlineVectorToList G.getInt64le len pos

instance VectorElement Float where
  data Vector Float = VectorFloat !Int32 !Position

  length (VectorFloat len _)       = len
  unsafeIndex (VectorFloat _ pos)  = readFloat . moveToElem pos floatSize
  take n (VectorFloat len pos)     = VectorFloat (clamp n len) pos
  drop n (VectorFloat len pos)     = VectorFloat (len - n') (moveToElem pos floatSize n')
    where n' = clamp n len
  toList (VectorFloat len pos)     = inlineVectorToList G.getFloatle len pos

instance VectorElement Double where
  data Vector Double = VectorDouble !Int32 !Position

  length (VectorDouble len _)      = len
  unsafeIndex (VectorDouble _ pos) = readDouble . moveToElem pos doubleSize
  take n (VectorDouble len pos)    = VectorDouble (clamp n len) pos
  drop n (VectorDouble len pos)    = VectorDouble (len - n') (moveToElem pos doubleSize n')
    where n' = clamp n len
  toList (VectorDouble len pos)    = inlineVectorToList G.getDoublele len pos

instance VectorElement Bool where
  data Vector Bool = VectorBool !Int32 !Position

  length (VectorBool len _)      = len
  unsafeIndex (VectorBool _ pos) = readBool . moveToElem pos boolSize
  take n (VectorBool len pos)    = VectorBool (clamp n len) pos
  drop n (VectorBool len pos)    = VectorBool (len - n') (moveToElem pos boolSize n')
    where n' = clamp n len
  toList (VectorBool len pos)    = fmap word8ToBool <$> toList (VectorWord8 len pos)

instance VectorElement Text where
  data Vector Text = VectorText !Int32 !Position

  length (VectorText len _)      = len
  unsafeIndex (VectorText _ pos) = readText . moveToElem pos textRefSize
  take n (VectorText len pos)    = VectorText (clamp n len) pos
  drop n (VectorText len pos)    = VectorText (len - n') (moveToElem pos textRefSize n')
    where n' = clamp n len

  toList :: Vector Text -> Either ReadError [Text]
  toList (VectorText len pos) = do
    offsets <- inlineVectorToList G.getInt32le len pos
    L.reverse <$> go offsets 0 []
    where
      go :: [Int32] -> Int32 -> [Text] -> Either ReadError [Text]
      go [] _ acc = Right acc
      go (offset : xs) ix acc = do
        let textPos = move pos (offset + (ix * 4))
        text <- join $ runGet textPos readText'
        go xs (ix + 1) (text : acc)

instance VectorElement (Struct a) where
  data Vector (Struct a) = VectorStruct
    { vectorStructStructSize :: !InlineSize
    , vectorStructLength     :: !Int32
    , vectorStructPos        :: !Position
    }

  length = vectorStructLength

  unsafeIndex (VectorStruct structSize _ pos) =
    let elemSize = fromIntegral @InlineSize @Int32 structSize
    in Right . readStruct . moveToElem pos elemSize

  take n (VectorStruct structSize len pos) = VectorStruct structSize (clamp n len) pos
  drop n (VectorStruct structSize len pos) = VectorStruct structSize (clamp (len - n) len) (moveToElem pos (fromIntegral @InlineSize @Int32 structSize) n)

  toList (VectorStruct structSize len pos) =
    Right (go len pos)
    where
      go :: Int32 -> Position -> [Struct a]
      go 0 _ = []
      go !len pos =
        let head = readStruct pos
            tail = go (len - 1) (move pos structSize)
        in  head : tail

instance VectorElement (Table a) where
  data Vector (Table a) = VectorTable !Int32 !PositionInfo


  length (VectorTable len _)      = len
  unsafeIndex (VectorTable _ pos) = readTable . moveToElem pos tableRefSize
  take n (VectorTable len pos)    = VectorTable (clamp n len) pos
  drop n (VectorTable len pos)    = VectorTable (len - n') (moveToElem pos tableRefSize n')
    where n' = clamp n len

  toList (VectorTable len vectorPos) = do
    offsets <- inlineVectorToList G.getInt32le len (getPosition vectorPos)
    go offsets 0
    where
      go :: [Int32] -> Int32 -> Either ReadError [Table a]
      go [] _ = Right []
      go (offset : offsets) !ix = do
        let tablePos = move vectorPos (offset + (ix * 4))
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
  length = length . vectorUnionTypesPos

  unsafeIndex (VectorUnion typesPos valuesPos readElem) ix = do
    unionType <- unsafeIndex typesPos ix
    case positive unionType of
      Nothing         -> Right UnionNone
      Just unionType' -> do
        tablePos <- readUOffsetAndSkip $ moveToElem valuesPos tableRefSize ix
        readElem unionType' tablePos

  take n (VectorUnion typesPos valuesPos readElem)     = VectorUnion (take n typesPos) valuesPos readElem
  drop n vec@(VectorUnion typesPos valuesPos readElem) = VectorUnion (drop n typesPos) (moveToElem valuesPos tableRefSize n') readElem
    where n' = clamp n (length vec)

  toList vec@(VectorUnion typesPos valuesPos readElem) = do
    unionTypes <- toList typesPos
    offsets <- inlineVectorToList G.getInt32le (length vec) (getPosition valuesPos)
    go unionTypes offsets 0
    where
      go :: [Word8] -> [Int32] -> Int32 -> Either ReadError [Union a]
      go [] [] _ = Right []
      go (unionType : unionTypes) (offset : offsets) !ix = do
        union <-
          case positive unionType of
            Nothing -> Right UnionNone
            Just unionType' ->
              let tablePos = move valuesPos (offset + (ix * 4))
              in  readElem unionType' tablePos
        unions <- go unionTypes offsets (ix + 1)
        pure (union : unions)
      go _ _ _ = Left "Union vector: 'type vector' and 'value vector' do not have the same length."

----------------------------------
----- Read from Struct/Table -----
----------------------------------
{-# INLINE readStructField #-}
readStructField :: (Position -> a) -> VOffset -> Struct s -> a
readStructField read voffset (Struct bs) =
  read (move bs voffset)

{-# INLINE readTableFieldOpt #-}
readTableFieldOpt :: (PositionInfo -> Either ReadError a) -> TableIndex -> Table t -> Either ReadError (Maybe a)
readTableFieldOpt read ix t = do
  mbOffset <- tableIndexToVOffset t ix
  traverse (\offset -> read (move (tablePos t) offset)) mbOffset

{-# INLINE readTableFieldReq #-}
readTableFieldReq :: (PositionInfo -> Either ReadError a) -> TableIndex -> String -> Table t -> Either ReadError a
readTableFieldReq read ix name t = do
  mbOffset <- tableIndexToVOffset t ix
  case mbOffset of
    Nothing     -> missingField name
    Just offset -> read (move (tablePos t) offset)

{-# INLINE readTableFieldWithDef #-}
readTableFieldWithDef :: (PositionInfo -> Either ReadError a) -> TableIndex -> a -> Table t -> Either ReadError a
readTableFieldWithDef read ix dflt t =
  tableIndexToVOffset t ix >>= \case
    Nothing -> Right dflt
    Just offset -> read (move (tablePos t) offset)

{-# INLINE readTableFieldUnion #-}
readTableFieldUnion :: (Positive Word8 -> PositionInfo -> Either ReadError (Union a)) -> TableIndex -> Table t -> Either ReadError (Union a)
readTableFieldUnion read ix t =
  readTableFieldWithDef readWord8 (ix - 1) 0 t >>= \unionType ->
    case positive unionType of
      Nothing         -> Right UnionNone
      Just unionType' ->
        tableIndexToVOffset t ix >>= \case
          Nothing     -> Left "Union: 'union type' found but 'union value' is missing."
          Just offset -> readUOffsetAndSkip (move (tablePos t) offset) >>= read unionType'

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
        Nothing -> Left "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          Just <$> readUnionVector read (move (tablePos t) typesOffset) (move (tablePos t) valuesOffset)

readTableFieldUnionVectorReq ::
     (Positive Word8 -> PositionInfo -> Either ReadError (Union a))
  -> TableIndex
  -> String
  -> Table t
  -> Either ReadError (Vector (Union a))
readTableFieldUnionVectorReq read ix name t =
  tableIndexToVOffset t (ix - 1) >>= \case
    Nothing -> missingField name
    Just typesOffset ->
      tableIndexToVOffset t ix >>= \case
        Nothing -> Left "Union vector: 'type vector' found but 'value vector' is missing."
        Just valuesOffset ->
          readUnionVector read (move (tablePos t) typesOffset) (move (tablePos t) valuesOffset)

----------------------------------
------ Read from `Position` ------
----------------------------------
{-# INLINE readInt8 #-}
readInt8 :: HasPosition a => a -> Either ReadError Int8
readInt8 (getPosition -> pos) = runGet pos G.getInt8

{-# INLINE readInt16 #-}
readInt16 :: HasPosition a => a -> Either ReadError Int16
readInt16 (getPosition -> pos) = runGet pos G.getInt16le

{-# INLINE readInt32 #-}
readInt32 :: HasPosition a => a -> Either ReadError Int32
readInt32 (getPosition -> pos) = runGet pos G.getInt32le

{-# INLINE readInt64 #-}
readInt64 :: HasPosition a => a -> Either ReadError Int64
readInt64 (getPosition -> pos) = runGet pos G.getInt64le

{-# INLINE readWord8 #-}
readWord8 :: HasPosition a => a -> Either ReadError Word8
readWord8 (getPosition -> pos) = runGet pos G.getWord8

{-# INLINE readWord16 #-}
readWord16 :: HasPosition a => a -> Either ReadError Word16
readWord16 (getPosition -> pos) = runGet pos G.getWord16le

{-# INLINE readWord32 #-}
readWord32 :: HasPosition a => a -> Either ReadError Word32
readWord32 (getPosition -> pos) = runGet pos G.getWord32le

{-# INLINE readWord64 #-}
readWord64 :: HasPosition a => a -> Either ReadError Word64
readWord64 (getPosition -> pos) = runGet pos G.getWord64le

{-# INLINE readFloat #-}
readFloat :: HasPosition a => a -> Either ReadError Float
readFloat (getPosition -> pos) = runGet pos G.getFloatle

{-# INLINE readDouble #-}
readDouble :: HasPosition a => a -> Either ReadError Double
readDouble (getPosition -> pos) = runGet pos G.getDoublele

{-# INLINE readBool #-}
readBool :: HasPosition a => a -> Either ReadError Bool
readBool p = word8ToBool <$> readWord8 p

{-# INLINE word8ToBool #-}
word8ToBool :: Word8 -> Bool
word8ToBool 0 = False
word8ToBool _ = True


readPrimVector ::
     (Int32 -> Position -> Vector a)
  -> PositionInfo
  -> Either ReadError (Vector a)
readPrimVector vecConstructor (posCurrent -> pos) = do
  vecPos <- readUOffsetAndSkip pos
  vecLength <- readInt32 vecPos
  Right $! vecConstructor vecLength (move vecPos (int32Size :: Int64))

readTableVector :: PositionInfo -> Either ReadError (Vector (Table a))
readTableVector pos = do
  vecPos <- readUOffsetAndSkip pos
  vecLength <- readInt32 vecPos
  Right $! VectorTable vecLength (move vecPos (int32Size :: Int64))

readStructVector :: forall a. IsStruct a => PositionInfo -> Either ReadError (Vector (Struct a))
readStructVector = readPrimVector (VectorStruct (structSizeOf @a))

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
      (move valuesVec (int32Size :: Int64))
      readUnion

-- | Follow a pointer to the position of a string and read it.
{-# INLINE readText #-}
readText :: HasPosition a => a -> Either ReadError Text
readText (getPosition -> pos) =
  join $ runGet pos $ do
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
    Left (T.DecodeError msg byteMaybe) ->
      case byteMaybe of
        Just byte -> Left $ "UTF8 decoding error (byte " <> show byte <> "): " <> msg
        Nothing   -> Left $ "UTF8 decoding error: " <> msg
    -- The `EncodeError` constructor is deprecated and not used
    -- https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Encoding-Error.html#t:UnicodeException
    Left _ -> error "the impossible happened"

-- | Follow a pointer to the position of a table and read it.
{-# INLINE readTable #-}
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

{-# INLINE readStruct #-}
readStruct :: HasPosition a => a -> Struct s
readStruct = Struct . getPosition

----------------------------------
---------- Primitives ------------
----------------------------------
{-# INLINE tableIndexToVOffset #-}
tableIndexToVOffset :: Table t -> TableIndex -> Either ReadError (Maybe VOffset)
tableIndexToVOffset Table{..} ix =
  runGet vtable $ do
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

{-# INLINE runGet #-}
runGet :: ByteString -> Get a -> Either ReadError a
runGet bs get =
  case G.runGetOrFail get bs of
    Right (_, _, a)  -> Right a
    Left (_, _, msg) -> Left msg

{-# NOINLINE missingField #-}
missingField :: String -> Either ReadError a
missingField fieldName =
  Left $ "Missing required table field: " <> fieldName

-- | Safer version of `Data.ByteString.Lazy.index` that doesn't throw when index is too large.
-- Assumes @i > 0@.

-- Adapted from `Data.ByteString.Lazy.index`: https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/src/Data.ByteString.Lazy.html#index
{-# INLINE byteStringSafeIndex #-}
byteStringSafeIndex :: ByteString -> Int32 -> Either ReadError Word8
byteStringSafeIndex !cs0 !i =
  index' cs0 i
  where index' BSL.Empty _ = Left "not enough bytes"
        index' (BSL.Chunk c cs) n
          -- NOTE: this might overflow in systems where Int has less than 32 bits
          | fromIntegral @Int32 @Int n >= BS.length c =
              -- Note: it's safe to narrow `BS.length` to an int32 here, the line above proves it.
              index' cs (n - fromIntegral @Int @Int32 (BS.length c))
          | otherwise = Right $! BSU.unsafeIndex c (fromIntegral @Int32 @Int n)
