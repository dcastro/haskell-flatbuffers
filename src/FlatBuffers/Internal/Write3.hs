{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Avoid lambda" #-}

module FlatBuffers.Internal.Write3 where

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad qualified as Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST (runST)
import Control.Monad.State.Strict as S
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Prim qualified as BSP
import Data.ByteString.Builder.Prim.Internal qualified as BSP
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce (coerce)
import Data.Foldable qualified as Fold
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int
import Data.IORef hiding (modifyIORef, writeIORef)
import Data.IORef qualified as IORef
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Internal qualified as MI
import Data.Map.Strict qualified as M
import Data.Map.Strict.Internal qualified as MSI
import Data.MonoTraversable
import Data.Primitive.ByteArray qualified as Prim
import Data.Semigroup (Max(..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Lazy qualified as LT
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VUB
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word
import Debug.Trace
import FlatBuffers.Internal.Build qualified as Build
import FlatBuffers.Internal.Constants
import FlatBuffers.Internal.Types
import Foreign.C.Types (CSize(CSize))
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils qualified as Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.Base (ByteArray#)
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple (pShowNoColor)
import Utils.Containers.Internal.StrictPair


-- #include "MachDeps.h"

-- $> import qualified FlatBuffers.Internal.Write as F

-- $> import qualified FlatBuffers.Internal.Read as F


 -- $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Int32 5 (\i -> pure (fromIntegral i * 3)) >> lift F.finish

 -- $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Text 10 (\i -> pure $ T.replicate (i + 1) "a") >> lift F.finish

 -- $> putStrLn $ F.showBuffer bs

{-


>>> import qualified FlatBuffers.Internal.Write3 as F

>>> let enc = prettyPrint . showBuffer . F.encodeDef


-- >>> F.showWrite2 do { loc <- F.writeText "abc"; F.writeTable 2 (F.writeInt32TableField 0 99 <> F.writeLocationTableField 1 loc); }
>>> F.showWrite2 do { F.writeTable 2 (F.writeInt32TableField 0 99); }
"6, 0, 8, 0
4, 0, 6, 0
0, 0, 99, 0
0, 0"

>>> enc do { F.writeTable 2 (F.writeInt32TableField 0 99); }
"12, 0, 0, 0
0, 0, 6, 0
8, 0, 4, 0
6, 0, 0, 0
99, 0, 0, 0"

>>> enc do { loc <- F.writeText "abc"; F.writeTable 2 (F.writeInt32TableField 0 99 <> F.writeLocationTableField 1 loc); }
"12, 0, 0, 0
8, 0, 12, 0
8, 0, 4, 0
8, 0, 0, 0
8, 0, 0, 0
99, 0, 0, 0
3, 0, 0, 0
97, 98, 99, 0"



 -}

data SmartPtr = SmartPtr
  { spPtr :: !(Ptr Word8)
  , spOffset :: !Word32 -- ^ Number of bytes between `spPtr` and the end of the buffer.
  }


-- Move the pointer to the right.
{-# INLINE plus #-}
plus :: SmartPtr -> Word32 -> SmartPtr
SmartPtr ptr offset `plus` n = SmartPtr (ptr `plusPtr` fromIntegral @Word32 @Int n) (offset - n)

-- Move the pointer to the left.
{-# INLINE minus #-}
minus :: SmartPtr -> Word32 -> SmartPtr
SmartPtr ptr offset `minus` n = SmartPtr (ptr `plusPtr` (-(fromIntegral @Word32 @Int n))) (offset + n)

moveSmartPtrM :: Int -> Write ()
moveSmartPtrM bytes = do
  modifyBuffer (\buffer -> moveSmartPtr buffer bytes)

moveSmartPtr :: Buffer -> Int -> Buffer
moveSmartPtr buffer bytes = do
  buffer { bufferSptr = buffer.bufferSptr `move` bytes }
  where
    move :: SmartPtr -> Int -> SmartPtr
    SmartPtr ptr offset `move` n = SmartPtr (ptr `plusPtr` n) (offset - fromIntegral @Int @Word32 n)


data Buffer = Buffer
  { bufferForeignPtr :: !(ForeignPtr Word8)
  , bufferSptr      :: !SmartPtr
  , bufferCapacity :: !Int
  , bufferMaxAlign :: !(Max Alignment)
  , bufferCache    :: !(M.Map BS.ByteString Word32)
  }

type BufferRef = IORef Buffer

bufferSize :: Buffer -> Word32
bufferSize buffer = buffer.bufferSptr.spOffset


-- TODO: write table field that was read using ExceptT

{-# INLINE writeTable #-}
writeTable :: Int -> WriteTableField -> Write (Location a)
writeTable fieldCount wtf = do
  buffer1 <- getBuffer
  let startFieldsLoc = bufferSize buffer1

  locs <- liftIO $ VUM.new @IO @Word32 fieldCount
  runWriteTableField wtf locs


  buffer2 <- getBuffer

  let endFieldsLoc = bufferSize buffer2
  let tableLoc = endFieldsLoc + 4
  let tableSize = tableLoc - startFieldsLoc
  let maxVtableSize = 2 + 2 + (2 * fieldCount)


  alignTo 4 0
  reserveM (4 + maxVtableSize)
  buffer <- getBuffer

  let
    skipTrailingZeroes :: Int -> IO Int
    skipTrailingZeroes index
      | index < 0 = pure index
      | otherwise = do
          -- traceM $ "skipTrailingZeroes: " <> show index
          loc <- VUM.unsafeRead locs index
          if loc == 0
            then skipTrailingZeroes (index - 1)
            else pure index

    writeTableOffsets :: Int -> SmartPtr -> IO SmartPtr
    writeTableOffsets index previousSptr
      | index < 0 = pure previousSptr
      | otherwise = do
          -- traceM $ "writeTableOffsets: " <> show index
          loc <- VUM.unsafeRead locs index
          let offset = if loc == 0 then 0 else tableLoc - loc
          let sptr = previousSptr `minus` 2
          putWord16 sptr (fromIntegral @Word32 @Word16 offset)
          writeTableOffsets (index - 1) sptr

  let sptr1 = bufferSptr buffer
  let tableSptr = sptr1 `minus` 4
  buffer <- liftIO $ do
    -- TODO: use `Write` as much as possible
    i <- skipTrailingZeroes (VUM.length locs - 1)
    sptr1 <- writeTableOffsets i tableSptr
    let vtableSptr = sptr1 `minus` 4
    let vtableSize = vtableSptr.spOffset - tableSptr.spOffset
    putWord16 vtableSptr (fromIntegral @Word32 @Word16 vtableSize)
    putWord16 (vtableSptr `plus` 2) (fromIntegral @Word32 @Word16 tableSize)

    -- TODO: change to the new `BS` constructor???
    let vtableBs = BSI.PS
          buffer.bufferForeignPtr
          (buffer.bufferCapacity - fromIntegral @Word32 @Int vtableSptr.spOffset)
          (fromIntegral @Word32 @Int vtableSize)

    case insertMap vtableBs vtableSptr.spOffset buffer.bufferCache of
      (Nothing, newCache) -> do
        -- No match was found - cache has been updated.
        -- Write offset to vtable.
        -- Note: the offset is always positive in this branch, as it points to the left.
        putInt32 tableSptr (fromIntegral @Word32 @Int32 vtableSize)

        pure buffer
          { bufferSptr = vtableSptr
          , bufferCache = newCache
          }

      (Just oldVtablePosition, _) -> do
        -- A match was found.
        -- Note: the offset is always negative in this branch, as it points to the right.
        putInt32 tableSptr $ fromIntegral @Word32 @Int32 oldVtablePosition - fromIntegral @Word32 @Int32 tableSptr.spOffset
        pure buffer
          { bufferSptr = tableSptr
          }


  putBuffer buffer

  pure $ Location tableSptr.spOffset


{-# INLINE writeInt8TableField #-}
writeInt8TableField :: Int -> Int8 -> WriteTableField
writeInt8TableField = writePrimitiveTableField int8Size putInt8

{-# INLINE writeInt16TableField #-}
writeInt16TableField :: Int -> Int16 -> WriteTableField
writeInt16TableField = writePrimitiveTableField int16Size putInt16

{-# INLINE writeInt32TableField #-}
writeInt32TableField :: Int -> Int32 -> WriteTableField
writeInt32TableField = writePrimitiveTableField int32Size putInt32

{-# INLINE writeInt64TableField #-}
writeInt64TableField :: Int -> Int64 -> WriteTableField
writeInt64TableField = writePrimitiveTableField int64Size putInt64

{-# INLINE writeWord8TableField #-}
writeWord8TableField :: Int -> Word8 -> WriteTableField
writeWord8TableField = writePrimitiveTableField word8Size putWord8

{-# INLINE writeWord16TableField #-}
writeWord16TableField :: Int -> Word16 -> WriteTableField
writeWord16TableField = writePrimitiveTableField word16Size putWord16

{-# INLINE writeWord32TableField #-}
writeWord32TableField :: Int -> Word32 -> WriteTableField
writeWord32TableField = writePrimitiveTableField word32Size putWord32

{-# INLINE writeWord64TableField #-}
writeWord64TableField :: Int -> Word64 -> WriteTableField
writeWord64TableField = writePrimitiveTableField word64Size putWord64

{-# INLINE writeFloatTableField #-}
writeFloatTableField :: Int -> Float -> WriteTableField
writeFloatTableField = writePrimitiveTableField floatSize putFloat

{-# INLINE writeDoubleTableField #-}
writeDoubleTableField :: Int -> Double -> WriteTableField
writeDoubleTableField = writePrimitiveTableField doubleSize putDouble

{-# INLINE writePrimitiveTableField #-}
writePrimitiveTableField
  :: forall field
   . Alignment
  -> (SmartPtr -> field -> IO ())
  -> Int
  -> field
  -> WriteTableField
writePrimitiveTableField alignment putFn fieldIndex fieldData = WriteTableField $ \locs -> do
  let fieldSize = fromIntegral @Alignment @Int alignment
  alignTo alignment fieldSize
  buffer <- getBuffer
  buffer <- pure $ moveSmartPtr buffer (-fieldSize)
  liftIO $ putFn buffer.bufferSptr fieldData
  liftIO $ VUM.unsafeWrite locs fieldIndex buffer.bufferSptr.spOffset
  putBuffer buffer

{-# INLINE writeBoolTableField #-}
writeBoolTableField :: Int -> Bool -> WriteTableField
writeBoolTableField fieldIndex = writeWord8TableField fieldIndex . Build.boolToWord8

writeLocationTableField :: Int -> Location a -> WriteTableField
writeLocationTableField fieldIndex loc = WriteTableField $ \locs -> do
  alignTo 4 4
  buffer <- getBuffer
  let sptr = buffer.bufferSptr `minus` 4
  let offsetToLocation = sptr.spOffset - loc.getLocation
  liftIO $ do
    putWord32 sptr offsetToLocation
    VUM.unsafeWrite locs fieldIndex sptr.spOffset
  putBuffer buffer { bufferSptr = sptr }

newtype WriteTableField = WriteTableField
  { runWriteTableField
      :: VUM.IOVector Word32
      -> Write ()
  }

missing :: WriteTableField
missing = WriteTableField $ const $ pure ()

deprecated :: WriteTableField
deprecated = missing

optional :: (Int -> a -> WriteTableField) -> (Int -> Maybe a -> WriteTableField)
optional writeTableField fieldIndex =
  maybe missing \a -> writeTableField fieldIndex a

optionalDef :: Eq a => a -> (Int -> a -> WriteTableField) -> (Int -> Maybe a -> WriteTableField)
optionalDef dflt writeTableField fieldIndex ma =
  case ma of
    Just a | a /= dflt -> writeTableField fieldIndex a
    _ -> missing

instance Semigroup WriteTableField where
  WriteTableField f <> WriteTableField g = WriteTableField $ \locs -> do
    f locs
    g locs

instance Monoid WriteTableField where
  mempty = WriteTableField $ \_ -> pure ()


-- | Modifed version of `insertLookupWithKey` that
-- 1) when a match is found, always keeps the old value
-- 2) when a match is found, forces a full copy of the bytestring key before storing it
{-# INLINE insertMap #-}
insertMap :: BS.ByteString -> a -> M.Map BS.ByteString a
                    -> (Maybe a, M.Map BS.ByteString a)
insertMap kx0 x0 t0 = toPair $ go kx0 x0 t0
  where
    go :: BS.ByteString -> a -> M.Map BS.ByteString a -> StrictPair (Maybe a) (M.Map BS.ByteString a)
    go !kx x MSI.Tip =
      let !bsCopy = BS.copy kx
      in  Nothing :*: MSI.singleton bsCopy x
    go kx x branch@(MSI.Bin _ ky y l r) =
        case compare kx ky of
            LT -> let (found :*: l') = go kx x l
                  in found :*: MI.balanceL ky y l' r
            GT -> let (found :*: r') = go kx x r
                  in found :*: MI.balanceR ky y l r'
            EQ -> Just y :*: branch

newtype Write a = Write { unsafeRunWrite :: ReaderT BufferRef IO a }
  deriving newtype (MonadIO, Functor, Applicative, Monad)

-- TODO: delete `MonadIO` and `MonadState Buffer` instances, re-add `fromIO`
-- Add a comment explaining why: we don't want users to be able to use `IO` in the `Write` monad,
-- we want the IO to be an implementation detail.
-- Not allowing the user to use IO lets us safely use `unsafePerformIO` in `runWrite`.


----------------------------------------------------------------------------
-- Experiments: allow using `Write` embded in any monad stack,
-- as long as it has `MonadState Buffer` and `MonadIO`.
----------------------------------------------------------------------------

usageExample :: ExceptT SomeException (ReaderT Int IO) BS.ByteString
usageExample = do
  bufferRef <- newBuffer defaultWriteSettings

  string <- liftWrite bufferRef $ writeText "abc"
  tableRoot <- liftWrite bufferRef $ writeTable 2 $
    writeInt32TableField 0 99
    <> writeLocationTableField 1 string

  encode' bufferRef tableRoot

liftWrite :: MonadIO m => BufferRef -> Write a -> m a
liftWrite bufferRef (Write action) =
  action
    & flip runReaderT bufferRef
    & liftIO

encode' :: MonadIO m => BufferRef -> Location a -> m BS.ByteString
encode' bufferRef tableRoot = do
  liftWrite bufferRef do
    writeTableRoot tableRoot
    finish

newBuffer :: MonadIO m => WriteSettings -> m BufferRef
newBuffer settings = do
  fp <- liftIO $ BSI.mallocByteString settings.initialCapacity
  let ptr = SmartPtr (unsafeForeignPtrToPtr fp `plusPtr` settings.initialCapacity) 0
  liftIO $ newIORef $ Buffer fp ptr settings.initialCapacity (Max 1) M.empty

--  $> import qualified FlatBuffers.Internal.Write as F

--  $> import qualified FlatBuffers.Internal.Read as F

--  $> :m +Control.Monad.Except

--  $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Int32 5 (\i -> pure (fromIntegral i * 3)) >> lift F.finish

--  $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Text 10 (\i -> pure $ T.replicate (i + 1) "a") >> lift F.finish

--  $> putStrLn $ F.showBuffer bs



--  $> :m +Control.Monad.Except

--  $> runWrite defaultWriteSettings . runExceptT . fmap showBuffer $ unfoldNM @Int32 4 (\i -> pure (fromIntegral @Int @Int32 $ i * 2)) >> lift finish

  -- $> F.showWrite $ do {
  -- $>   loc <- F.writeText "abc";
  -- $>   F.writeTable 2 (F.writeInt32TableField 0 99 <> F.writeLocationTableField 1 loc);
  -- $> }


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

encodePerson :: BS.ByteString
encodePerson =
  encode defaultWriteSettings do
    writeTable 3 $ mconcat
      [
        writeInt32TableField 0 11
      ,
        optional writeInt32TableField 1 Nothing
      ,
        writeInt32TableField 2 22
      ]

{-
>>> import Data.ByteString qualified as BS
>>> people = [Person "bbb" 55, Person "aaa" 44]

>>> BS.writeFile "3.bin" $ encodePeople2 people

>>> prettyBuffer $ encodePeople2 people
"12, 0, 0, 0
0, 0, 6, 0
8, 0, 4, 0
6, 0, 0, 0
4, 0, 0, 0
2, 0, 0, 0
36, 0, 0, 0
4, 0, 0, 0
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

encodePeople2 :: [Person] -> BS.ByteString
encodePeople2 people =
  encode defaultWriteSettings do

    peopleTables :: VU.Vector (Location Person) <- writeMany people \person -> do
      name <- writeText person.personName
      writeTable @Person 2 $ mconcat
        [
          writeInt32TableField 0 person.personAge
          ,
          writeLocationTableField 1 name
        ]

    peopleVector <- toVector peopleTables

    writeTable 1 $ writeLocationTableField 0 peopleVector

encodeWeapons :: [Either Text Int32] -> BS.ByteString
encodeWeapons weapons = do
  encode defaultWriteSettings do

    (unionLocs, unionTypes) <- writeMany weapons \case
      Left str -> do
        text <- writeText str
        tableLoc <- writeTable 1 $ writeLocationTableField 0 text
        pure $ UnionLocation 1 tableLoc
      Right int -> do
        tableLoc <- writeTable 1 $ writeInt32TableField 0 int
        pure $ UnionLocation 2 tableLoc

    unionLocsVec <- toVector unionLocs
    unionTypesVec <- toVector unionTypes

    writeTable 2 $ mconcat
      [ writeLocationTableField 0 unionTypesVec
      , writeLocationTableField 1 unionLocsVec
      ]
{-

>>> BS.writeFile "weapons3.bin" $ encodeWeapons [Left "aa", Right 11]

>>> prettyBuffer $ encodeWeapons [Left "aa", Right 11]
"12, 0, 0, 0
8, 0, 12, 0
8, 0, 4, 0
8, 0, 0, 0
16, 0, 0, 0
4, 0, 0, 0
2, 0, 0, 0
1, 2, 0, 0
2, 0, 0, 0
32, 0, 0, 0
12, 0, 0, 0
0, 0, 6, 0
10, 0, 4, 0
6, 0, 0, 0
11, 0, 0, 0
0, 0, 6, 0
8, 0, 4, 0
6, 0, 0, 0
4, 0, 0, 0
2, 0, 0, 0
97, 97, 0, 0"


-}

class WriteMany loc where
  type Many loc :: Type
  writeMany
    :: forall elem mono. (MonoFoldable mono, Element mono ~ elem)
    => mono
    -> (elem -> Write loc)
    -> Write (Many loc)

data UnionLocation tag = UnionLocation
  { ulType :: !(UnionType tag)
  , ulLocation :: !(Location tag)
  }

newtype UnionType tag = UnionType { getUnionType :: Word8 }
  deriving newtype (Num)

newtype instance VU.MVector s (UnionType a) = MV_Word8 (VP.MVector s Word8)
newtype instance VU.Vector    (UnionType a) = V_Word8  (VP.Vector    Word8)
deriving via (VU.UnboxViaPrim Word8) instance VGM.MVector VU.MVector (UnionType a)
deriving via (VU.UnboxViaPrim Word8) instance VG.Vector   VU.Vector  (UnionType a)
instance VU.Unbox (UnionType a)

instance WriteMany (UnionLocation a) where
  type Many (UnionLocation a) = (VU.Vector (Location a), VU.Vector (UnionType a))

  {-# INLINE writeMany #-}
  writeMany
    :: forall elem a mono. (MonoFoldable mono, Element mono ~ elem)
    => mono
    -> (elem -> Write (UnionLocation a))
    -> Write (VU.Vector (Location a), VU.Vector (UnionType a))
  writeMany collection writeElem = do
    let elemCount = olength collection
    elemLocations <- liftIO $ VUM.new @IO @(Location a) elemCount
    unionTypes <- liftIO $ VUM.new @IO @(UnionType a) elemCount

    let
      writeOneElem :: Int -> elem -> Write Int
      writeOneElem currentIndex elem = do
        unionLoc <- writeElem elem
        liftIO do
          VUM.unsafeWrite elemLocations currentIndex unionLoc.ulLocation
          VUM.unsafeWrite unionTypes currentIndex unionLoc.ulType
          pure $ currentIndex + 1
    _ <- ofoldM writeOneElem 0 collection

    liftIO $ (,)
      <$> VU.unsafeFreeze elemLocations
      <*> VU.unsafeFreeze unionTypes

instance WriteMany (Location a) where
  type Many (Location a) = VU.Vector (Location a)

  {-# INLINE writeMany #-}
  writeMany
    :: forall elem a mono. (MonoFoldable mono, Element mono ~ elem)
    => mono
    -> (elem -> Write (Location a))
    -> Write (VU.Vector (Location a))
  writeMany collection writeElem = do

    let elemCount = olength collection
    elemLocations <- liftIO $ VUM.new @IO @(Location a) elemCount

    let
      writeOneElem :: Int -> elem -> Write Int
      writeOneElem currentIndex elem = do
        loc <- writeElem elem
        liftIO do
          VUM.unsafeWrite elemLocations currentIndex loc
          pure $ currentIndex + 1
    _ <- ofoldM writeOneElem 0 collection

    liftIO $ VU.unsafeFreeze elemLocations

class ToVector collection where
  type Elem collection
  toVector :: collection -> Write (Location [Elem collection])

newtype ToVectorViaFoldable collection a = ToVectorViaFoldable (collection a)

deriving via (ToVectorViaFoldable [] (Location a)) instance ToVector [Location a]

instance ToVector (VU.Vector (Location a)) where
  type Elem (VU.Vector (Location a)) = a
  toVector :: VU.Vector (Location a) -> Write (Location [a])
  toVector collection =
    genericToVector @(VU.Vector (Location a)) @(Location a) word32Size (VU.length collection) collection VU.foldM' putLocation

instance Foldable collection => ToVector (ToVectorViaFoldable collection (Location a)) where
  type Elem (ToVectorViaFoldable collection (Location a)) = a
  toVector :: ToVectorViaFoldable collection (Location a) -> Write (Location [a])
  toVector (ToVectorViaFoldable collection) =
    genericToVector @(collection (Location a)) @(Location a) word32Size (Fold.length collection) collection Monad.foldM putLocation

instance Foldable collection => ToVector (ToVectorViaFoldable collection Word8) where
  type Elem (ToVectorViaFoldable collection Word8) = Word8
  toVector :: ToVectorViaFoldable collection Word8 -> Write (Location [Word8])
  toVector (ToVectorViaFoldable collection) =
    genericToVector @(collection Word8) @Word8 word8Size (Fold.length collection) collection Monad.foldM putWord8

instance Foldable collection => ToVector (ToVectorViaFoldable collection Word16) where
  type Elem (ToVectorViaFoldable collection Word16) = Word16
  toVector :: ToVectorViaFoldable collection Word16 -> Write (Location [Word16])
  toVector (ToVectorViaFoldable collection) =
    genericToVector @(collection Word16) @Word16 word16Size (Fold.length collection) collection Monad.foldM putWord16

instance Foldable collection => ToVector (ToVectorViaFoldable collection Word32) where
  type Elem (ToVectorViaFoldable collection Word32) = Word32
  toVector :: ToVectorViaFoldable collection Word32 -> Write (Location [Word32])
  toVector (ToVectorViaFoldable collection) =
    genericToVector @(collection Word32) @Word32 word32Size (Fold.length collection) collection Monad.foldM putWord32


instance ToVector (VU.Vector (UnionType a)) where
  type Elem (VU.Vector (UnionType a)) = UnionType a
  toVector :: VU.Vector (UnionType a) -> Write (Location [UnionType a])
  toVector = coerce $ toVector @(VP.Vector Word8)

deriving newtype instance ToVector (VU.Vector Word8)
deriving newtype instance ToVector (VU.Vector Word16)
deriving newtype instance ToVector (VU.Vector Word32)
deriving via (ToVectorViaFoldable [] Word8) instance ToVector [Word8]
deriving via (ToVectorViaFoldable [] Word16) instance ToVector [Word16]
deriving via (ToVectorViaFoldable [] Word32) instance ToVector [Word32]


instance ToVector (VP.Vector Word8) where
  type Elem (VP.Vector Word8) = Word8
  toVector :: VP.Vector Word8 -> Write (Location [Word8])
  toVector vec@(VP.Vector off len byteArray) = do
    genericToVectorMemcpy word8Size (VP.length vec) byteArray off len

instance ToVector (VP.Vector Word16) where
  type Elem (VP.Vector Word16) = Word16
  toVector :: VP.Vector Word16 -> Write (Location [Word16])
#ifdef WORDS_BIGENDIAN
  toVector vec = do
    genericToVector word16Size (VP.length vec) vec VP.foldM' putWord16
#else
  toVector vec@(VP.Vector off len byteArray) = do
    genericToVectorMemcpy word16Size (VP.length vec) byteArray off len
#endif

instance ToVector (VP.Vector Word32) where
  type Elem (VP.Vector Word32) = Word32
  toVector :: VP.Vector Word32 -> Write (Location [Word32])
#ifdef WORDS_BIGENDIAN
  toVector vec = do
    genericToVector word32Size (VP.length vec) vec VP.foldM' putWord32
#else
  toVector vec@(VP.Vector off len byteArray) = do
    genericToVectorMemcpy word32Size (VP.length vec) byteArray off len
  -- toVector vec@(VP.Vector off len byteArray) = do
  --   -- Reserve the total amount of bytes needed to write the vector and align the buffer.
  --   let vectorByteCount = word32Size + (VP.length vec * word32Size)
  --   alignTo word32Size vectorByteCount
  --   moveSmartPtrM (-vectorByteCount)
  --   -- TODO: write vector count
  --   buffer <- getBuffer
  --   liftIO $ Prim.copyByteArrayToAddr buffer.bufferSptr.spPtr byteArray off len
  --   getCurrentLocation
#endif

-- | This function assumes the collection's length can be calculated in O(1).
--
-- Moves the `bufferSptr` to the start of the vector's location.
-- TODO: delete this in favor of `toVector`
{-# INLINE writeVector #-}
writeVector
  :: forall coll a
   . (MonoFoldable coll, Element coll ~ a)
  => Int
  -> coll
  -> (SmartPtr -> a -> IO ())
  -> Write ()
writeVector elemSize collection writeElem = do
  let vectorByteCount = int32Size + (len * elemSize)
  alignTo (4 `max` fromIntegral @Int @Alignment elemSize) vectorByteCount
  moveSmartPtrM (-vectorByteCount)
  writeCount
  writeElems
  moveSmartPtrM (-vectorByteCount)
  where
    len = olength collection

    writeElems :: Write ()
    writeElems = do
      buffer <- getBuffer
      newSptr <- liftIO $ ofoldM writeOneElem buffer.bufferSptr collection
      putBuffer $ buffer { bufferSptr = newSptr }

    writeOneElem :: SmartPtr -> a -> IO SmartPtr
    writeOneElem sptr elem = do
      writeElem sptr elem
      pure $ sptr `plus` fromIntegral @Int @Word32 elemSize

    writeCount :: Write ()
    writeCount = do
      buffer <- getBuffer
      liftIO $ putInt32 buffer.bufferSptr (fromIntegral @Int @Int32 len)
      putBuffer $ moveSmartPtr buffer int32Size

-- | Copies a bytearray into the buffer in O(1).
-- Moves the pointer to the start of the vector's location.
genericToVectorMemcpy
  :: Int
  -> Int
  -> Prim.ByteArray
  -> Int
  -> Int
  -> Write (Location x)
genericToVectorMemcpy elemSize collectionLength byteArray byteArrayOffset byteArrayLength = do
  -- Reserve the total amount of bytes needed to write the vector and align the buffer.
  let vectorByteCount = word32Size + (collectionLength * elemSize)
  alignTo (word32Size `max` fromIntegral @Int @Alignment elemSize) vectorByteCount
  moveSmartPtrM (-vectorByteCount)

  -- Write vector count
  buffer1 <- getBuffer
  liftIO $ putWord32 buffer1.bufferSptr (fromIntegral @Int @Word32 collectionLength)
  let buffer2 = moveSmartPtr buffer1 word32Size

  -- Memcpy
  liftIO $ Prim.copyByteArrayToAddr buffer2.bufferSptr.spPtr byteArray byteArrayOffset byteArrayLength

  let location = getBufferLocation buffer1
  putBuffer buffer1
  pure location


-- | Copies the elements of a source collection into the buffer one by one.
--
-- Moves the pointer to the start of the vector's location.
{-# INLINE genericToVector #-}
genericToVector
  :: forall collection elem x
   . Word32
  -> Int
  -> collection
  -> ((SmartPtr -> elem -> IO SmartPtr) -> SmartPtr -> collection -> IO SmartPtr)
  -> (SmartPtr -> elem -> IO ())
  -> Write (Location x)
genericToVector elemSize collectionLength collection foldMfunction writeElem = do
  let vectorByteCount = word32Size + (collectionLength * fromIntegral @Word32 @Int elemSize)
  alignTo (word32Size `max` fromIntegral @Word32 @Alignment elemSize) vectorByteCount
  moveSmartPtrM (-vectorByteCount)
  writeCount
  writeElems
  moveSmartPtrM (-vectorByteCount)
  getCurrentLocation
  where
    writeElems :: Write ()
    writeElems = do
      buffer <- getBuffer
      newSptr <- liftIO $ foldMfunction writeOneElem buffer.bufferSptr collection
      putBuffer $ buffer { bufferSptr = newSptr }

    writeOneElem :: SmartPtr -> elem -> IO SmartPtr
    writeOneElem sptr elem = do
      writeElem sptr elem
      pure $ sptr `plus` elemSize

    writeCount :: Write ()
    writeCount = do
      buffer <- getBuffer
      liftIO $ putWord32 buffer.bufferSptr (fromIntegral @Int @Word32 collectionLength)
      putBuffer $ moveSmartPtr buffer word32Size

{-# INLINE getCurrentLocation #-}
getCurrentLocation :: Write (Location a)
getCurrentLocation = getBufferLocation <$> getBuffer

{-# INLINE getBufferLocation #-}
getBufferLocation :: Buffer -> Location a
getBufferLocation = Location . bufferSize

{-# INLINE getBuffer #-}
getBuffer :: Write Buffer
getBuffer = getBufferRef >>= liftIO . readIORef

{-# INLINE getBufferRef #-}
getBufferRef :: Write BufferRef
getBufferRef = Write ask

{-# INLINE putBuffer #-}
putBuffer :: Buffer -> Write ()
putBuffer b = do
  bufferRef <- getBufferRef
  liftIO $ writeIORef bufferRef b

-- | A version of `IORef.writeIORef` that evaluates its argument to WHNF before writing it to the `IORef`.
-- This helps avoid the building up of thunks.
writeIORef :: IORef a -> a -> IO ()
writeIORef ioref !a = IORef.writeIORef ioref a

modifyBuffer :: (Buffer -> Buffer) -> Write ()
modifyBuffer f = do
  bufferRef <- getBufferRef
  liftIO $ modifyIORef' bufferRef f

writeUOffsetFrom :: Location a -> Write ()
writeUOffsetFrom loc = do
  alignTo uoffsetSize 0
  currentLoc <- getCurrentLocation
  let uoffset = currentLoc.getLocation - loc.getLocation + uoffsetSize

  buffer <- getBuffer
  buffer <- pure $ moveSmartPtr buffer (-word32Size)
  liftIO $ putWord32 buffer.bufferSptr uoffset
  putBuffer buffer

encode :: WriteSettings -> Write (Location a) -> BS.ByteString
encode settings writeTable =
  runWrite settings do
    tableRoot <- writeTable
    writeTableRoot tableRoot
    finish

writeTableRoot :: Location a -> Write ()
writeTableRoot tableRoot = do
  maxAlignment <- getBuffer <&> getMax . bufferMaxAlign
  alignTo maxAlignment uoffsetSize
  writeUOffsetFrom tableRoot

encodeDef :: Write (Location a) -> BS.ByteString
encodeDef = encode defaultWriteSettings

finish :: Write BS.ByteString
finish = do
  buffer <- getBuffer
  liftIO $ touchForeignPtr (bufferForeignPtr buffer)

  let size = fromIntegral @Word32 @Int $ bufferSize buffer
  let offset = buffer.bufferCapacity - size
  pure $ BSI.PS buffer.bufferForeignPtr offset size

{-# INLINE runWrite #-}
runWrite :: WriteSettings -> Write a -> a
runWrite (WriteSettings initialCapacity) write = unsafePerformIO $ do
  fp <- BSI.mallocByteString initialCapacity
  let ptr = SmartPtr (unsafeForeignPtrToPtr fp `plusPtr` initialCapacity) 0

  initialBufferRef <- newIORef $ Buffer fp ptr initialCapacity (Max 1) M.empty

  runReaderT (unsafeRunWrite write) initialBufferRef


data WriteSettings = WriteSettings
  { initialCapacity :: !Int
  -- TODO: file identifier
  }

defaultWriteSettings :: WriteSettings
defaultWriteSettings = WriteSettings
  { initialCapacity = 1024
  }

withInitialCapacity :: Int -> WriteSettings -> WriteSettings
withInitialCapacity n ws = ws { initialCapacity = n }


{-# INLINE reserveM #-}
reserveM :: Int -> Write ()
reserveM bytes = Write $ do
  bufferRef <- ask
  liftIO do
    buffer <- readIORef bufferRef
    buffer <- reserve bytes buffer
    writeIORef bufferRef buffer

{-# INLINE reserve #-}
reserve :: Int -> Buffer -> IO Buffer
reserve bytes buffer = do
  let (Buffer fp sptr capacity _ _) = buffer
  let size = bufferSize buffer
  let size' = fromIntegral @Word32 @Int size
  if capacity >= size' + bytes
    then pure buffer
    else do
      -- TODO: CHECK FOR INT32 OVERFLOWS
      -- (maybe cast to an unbound Int, and then check if it's > maxBound @Int32)?
      let newCapacity = (capacity * 2) `max` (capacity + bytes)

      -- Allocate new buffer and copy over the contents of the previous buffer
      newFp <- BSI.mallocByteString newCapacity
      let newPtr = unsafeForeignPtrToPtr newFp `plusPtr` (newCapacity - size')
      Marshal.copyBytes newPtr (spPtr sptr) size'


      -- TODO: try to have just 1 buffer field in `Buffer`,
      -- to avoid using `unsafeForeignPtrToPtr` and `touchForeignPtr`

      -- Make sure the previous `ForeignPtr` lives at least up until this point,
      -- to avoid invalidating its `Ptr`.
      -- See: https://hackage.haskell.org/package/base-4.12.0.0/docs/Foreign-ForeignPtr-Unsafe.html#v:unsafeForeignPtrToPtr
      touchForeignPtr fp

      pure $ buffer
            { bufferForeignPtr = newFp
            , bufferSptr = SmartPtr newPtr size
            , bufferCapacity = newCapacity
            }

-- | Reserves at least @additionalBytes@ bytes and adds enough 0-padding so
-- that the buffer becomes aligned to @n@ after writing @additionalBytes@.
--
-- Moves the pointer to the position before the padding.
{-# INLINE alignTo #-}
alignTo :: Alignment{- ^ n -} -> Int {- ^ additionalBytes -} -> Write ()
alignTo !n !additionalBytes = do
  bsize <- fromIntegral @Word32 @Int . bufferSize <$> getBuffer
  let padding = calcPadding n additionalBytes bsize
  reserveM (padding + additionalBytes)
  if padding == 0
    then
      modifyBuffer $ \b -> b { bufferMaxAlign = bufferMaxAlign b <> Max n }
    else do
      buffer <- getBuffer
      let newSptr = bufferSptr buffer `minus` fromIntegral @Int @Word32 padding
      _ <- liftIO $ Marshal.fillBytes (spPtr newSptr) 0 padding

      putBuffer buffer
        { bufferSptr = newSptr
        , bufferMaxAlign = bufferMaxAlign buffer <> Max n
        }

-- | Calculate how much 0-padding is needed so that, after writing @additionalBytes@,
-- the buffer becomes aligned to @n@ bytes.

-- TODO: change the args to Word32?
{-# INLINE calcPadding #-}
calcPadding :: Alignment {- ^ n -} -> Int {- ^ additionalBytes -} -> Int -> Int
calcPadding !n !additionalBytes bufferSize =
  (complement (bufferSize + additionalBytes) + 1) .&. (fromIntegral n - 1)

{-# INLINE putInt8 #-}
putInt8 :: SmartPtr -> Int8 -> IO ()
putInt8 sptr x = BSP.runF BSP.int8 x sptr.spPtr

{-# INLINE putInt16 #-}
putInt16 :: SmartPtr -> Int16 -> IO ()
putInt16 sptr x = BSP.runF BSP.int16LE x sptr.spPtr

{-# INLINE putInt32 #-}
putInt32 :: SmartPtr -> Int32 -> IO ()
putInt32 sptr x = BSP.runF BSP.int32LE x sptr.spPtr

{-# INLINE putInt64 #-}
putInt64 :: SmartPtr -> Int64 -> IO ()
putInt64 sptr x = BSP.runF BSP.int64LE x sptr.spPtr

{-# INLINE putWord8 #-}
putWord8 :: SmartPtr -> Word8 -> IO ()
putWord8 sptr x = BSP.runF BSP.word8 x sptr.spPtr

{-# INLINE putWord16 #-}
putWord16 :: SmartPtr -> Word16 -> IO ()
putWord16 sptr x = BSP.runF BSP.word16LE x sptr.spPtr

{-# INLINE putWord32 #-}
putWord32 :: SmartPtr -> Word32 -> IO ()
putWord32 sptr x = BSP.runF BSP.word32LE x sptr.spPtr

{-# INLINE putWord64 #-}
putWord64 :: SmartPtr -> Word64 -> IO ()
putWord64 sptr x = BSP.runF BSP.word64LE x sptr.spPtr

{-# INLINE putFloat #-}
putFloat :: SmartPtr -> Float -> IO ()
putFloat sptr x = BSP.runF BSP.floatLE x sptr.spPtr

{-# INLINE putDouble #-}
putDouble :: SmartPtr -> Double -> IO ()
putDouble sptr x = BSP.runF BSP.doubleLE x sptr.spPtr

{-# INLINE putLocation #-}
putLocation :: SmartPtr -> Location a -> IO ()
putLocation sptr loc = do
  let currentLoc = sptr.spOffset
  let uoffset = currentLoc - loc.getLocation
  putWord32 sptr uoffset

writeText :: Text -> Write (Location Text)
writeText text@(TI.Text arr off len) = do
  bsize <- fromIntegral @Word32 @Int . bufferSize <$> getBuffer
  let utf8len = utf8length text
  let utf8lenAndTerminator = utf8len + 1
  let pad = calcPadding int32Size utf8lenAndTerminator bsize
  let padAndTerminator = pad + 1
  let totalBytes = int32Size + utf8lenAndTerminator + pad

  reserveM totalBytes
  buffer <- getBuffer

  newSptr <- liftIO $ do
    let sptr1 = bufferSptr buffer

    let sptr2 = sptr1 `minus` fromIntegral @Int @Word32 padAndTerminator
    Marshal.fillBytes (spPtr sptr2) 0 (fromIntegral padAndTerminator)

    let sptr3 = sptr2 `minus` fromIntegral @Int @Word32 utf8len
    let !_ = runST $ A.copyToPointer arr off sptr3.spPtr len

    let sptr4 = sptr3 `minus` int32Size
    putInt32 sptr4 (fromIntegral @Int @Int32 utf8len)

    pure sptr4

  let newBuffer = buffer
        { bufferSptr = newSptr
        , bufferMaxAlign = bufferMaxAlign buffer <> Max int32Size
        }
  putBuffer newBuffer

  pure (Location (bufferSize newBuffer))

-- TODO: rename to `Reference`
newtype Location a = Location { getLocation :: Word32 }
  deriving newtype (Eq, Show)


newtype instance VU.MVector s (Location a) = MV_Word32 (VP.MVector s Word32)
newtype instance VU.Vector    (Location a) = V_Word32  (VP.Vector    Word32)
deriving via (VU.UnboxViaPrim Word32) instance VGM.MVector VU.MVector (Location a)
deriving via (VU.UnboxViaPrim Word32) instance VG.Vector   VU.Vector  (Location a)
instance VU.Unbox (Location a)

{-# INLINE utf8length #-}
utf8length :: Text -> Int
utf8length (TI.Text _array _offset len) = len


----------------------------------------------------------------------------
-- Debugging
----------------------------------------------------------------------------


enc :: Write (Location a) -> PrettyString
enc = prettyBuffer . encodeDef

prettyBuffer :: BS.ByteString -> PrettyString
prettyBuffer = prettyPrint . showBuffer

showBuffer :: BS.ByteString -> String
showBuffer bs =
  List.intercalate "\n" . fmap (List.intercalate ", ") . groupsOf 4 . fmap show $
  BS.unpack bs

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs =
  case take n xs of
    [] -> []
    group -> group : groupsOf n (drop n xs)


{- A pretty printer to be used in doctests.

Unlike @doctest@, HLS's @eval@ plugin does not capture stdout.
This means using `print` or `Text.Pretty.Simple.pPrint` won't work;
the `eval` plugin will not display the printed text.

The workaround mentioned in the docs (see below) is
not compatible with doctest: https://github.com/haskell/haskell-language-server/issues/1977#issuecomment-1635508324

This function works around the issue by overloading `show` such that the string is not wrapped in quotes
and newlines (and other characters) are not escaped.

In other words, whereas a `String` will be displayed with quotes/escape characters in GHCI,
a `PrettyString` will be rendered verbatim.

See:
  * Suggested workaround: https://github.com/haskell/haskell-language-server/blob/fb5e5c998c7d4f13546ae015191a7983aedf3345/plugins/hls-eval-plugin/README.md#multiline-output
-}
prettyPrint :: Show a => a -> PrettyString
prettyPrint a = PrettyString $ pShowNoColor a

newtype PrettyString = PrettyString LT.Text

instance Show PrettyString where
  show (PrettyString text) = LT.unpack text
