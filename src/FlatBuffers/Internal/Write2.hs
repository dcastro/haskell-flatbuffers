{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module FlatBuffers.Internal.Write2 where

import Data.List qualified as List

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict as S
import Foreign.C.Types (CSize(CSize))
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable

import Foreign.Marshal.Utils (with)

import Data.Map.Internal qualified as MI
import Data.Map.Strict qualified as M
import Data.Map.Strict.Internal qualified as MSI
import Utils.Containers.Internal.StrictPair


import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
-- import qualified Data.ByteString.Builder       as B
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU

import Data.Int
import Data.Semigroup (Max(..))
import Data.Word
import Debug.Trace

import Data.Coerce (coerce)
import FlatBuffers.Internal.Constants
import FlatBuffers.Internal.Types

import System.IO.Unsafe (unsafePerformIO)

import Data.Bits


import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import GHC.Base (ByteArray#)


import Data.Text.Array qualified as A
import Data.Text.Internal qualified as TI

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM


#include "MachDeps.h"

-- $> import qualified FlatBuffers.Internal.Write as F

-- $> import qualified FlatBuffers.Internal.Read as F


 -- $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Int32 5 (\i -> pure (fromIntegral i * 3)) >> lift F.finish

 -- $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Text 10 (\i -> pure $ T.replicate (i + 1) "a") >> lift F.finish

 -- $> putStrLn $ F.showBuffer' bs

data SmartPtr = SmartPtr
  { spPtr :: !(Ptr Word8)
  , spOffset :: !Int
  }


{-# INLINE plus #-}
plus :: SmartPtr -> Int -> SmartPtr
SmartPtr ptr offset `plus` n = SmartPtr (ptr `plusPtr` n) (offset - n)

{-# INLINE minus #-}
minus :: SmartPtr -> Int -> SmartPtr
SmartPtr ptr offset `minus` n = SmartPtr (ptr `plusPtr` (-n)) (offset + n)

{-# INLINE diff #-}
diff :: SmartPtr -> SmartPtr -> Int
SmartPtr _ offset1 `diff` SmartPtr _ offset2 = offset1 - offset2



data Buffer = Buffer
  { bufferForeignPtr :: !(ForeignPtr Word8)
  , bufferSptr      :: !SmartPtr
  , bufferCapacity :: !Int
  , bufferMaxAlign :: !(Max Alignment)
  , bufferCache    :: !(M.Map BS.ByteString Int)
  }

bufferSize :: Buffer -> Int
bufferSize = spOffset . bufferSptr


-- TODO: write table field that was read using ExceptT

{-# INLINE writeTable #-}
writeTable :: Int -> WriteTableField -> Write (Location a)
writeTable fieldCount wtf = do
  buffer1 <- getBuffer
  let startFieldsLoc = bufferSize buffer1

  locs <- liftIO $ VUM.new @IO @Int fieldCount
  runWriteTableField wtf locs


  buffer2 <- getBuffer

  let endFieldsLoc = bufferSize buffer2
  let tableLoc = endFieldsLoc + 4
  let tableSize = tableLoc - startFieldsLoc
  let maxVtableSize = 2 + 2 + (2 * fieldCount)


  alignTo 4 0
  reserve (4 + maxVtableSize)
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
          putWord16 sptr (fromIntegral @Int @Word16 offset)
          writeTableOffsets (index - 1) sptr

  let sptr1 = bufferSptr buffer
  let tableSptr = sptr1 `minus` 4
  buffer <- liftIO $ do
    -- TODO: use `Write` as much as possible
    i <- skipTrailingZeroes (VUM.length locs - 1)
    sptr1 <- writeTableOffsets i tableSptr
    let vtableSptr = sptr1 `minus` 4
    let vtableSize = vtableSptr `diff` tableSptr
    putWord16 vtableSptr (fromIntegral @Int @Word16 vtableSize)
    putWord16 (vtableSptr `plus` 2) (fromIntegral @Int @Word16 tableSize)

    let vtableBs = BSI.PS
          (bufferForeignPtr buffer)
          (bufferCapacity buffer - spOffset vtableSptr)
          vtableSize

    case insertMap vtableBs (spOffset vtableSptr) (bufferCache buffer) of
      (Nothing, newCache) -> do
        -- no match was found - cache has been updated
        -- write offset to vtable
        putInt32 tableSptr (fromIntegral @Int @Int32 vtableSize)

        pure buffer
          { bufferSptr = vtableSptr
          , bufferCache = newCache
          }

      (Just oldVtablePosition, _) -> do
        -- a match was found
        putInt32 tableSptr (fromIntegral @Int @Int32 (oldVtablePosition - spOffset tableSptr))
        pure buffer
          { bufferSptr = tableSptr
          }


  putBuffer buffer

  pure $ Location $ spOffset tableSptr


{-# INLINE writeInt32 #-}
writeInt32 :: Int -> Int32 -> WriteTableField
writeInt32 fieldIndex i = WriteTableField $ \locs -> do
  alignTo 4 4
  buffer <- getBuffer
  let sptr = bufferSptr buffer `minus` 4
  liftIO $ do
    putInt32 sptr i
    VUM.unsafeWrite locs fieldIndex (spOffset sptr)
  putBuffer buffer { bufferSptr = sptr }


writeOffset :: Int -> Location a -> WriteTableField
writeOffset fieldIndex loc = WriteTableField $ \locs -> do
  alignTo 4 4
  buffer <- getBuffer
  let sptr = bufferSptr buffer `minus` 4
  let offsetToLocation = spOffset sptr - getLocation loc
  liftIO $ do
    putInt32 sptr (fromIntegral @Int @Int32 offsetToLocation)
    VUM.unsafeWrite locs fieldIndex (spOffset sptr)
  putBuffer buffer { bufferSptr = sptr }

showWriteT :: ExceptT e Write a -> IO ()
showWriteT w =
  let Right bs = runWrite defaultWriteSettings $ runExceptT $ w >> lift finish
  in  putStrLn $ showBuffer' bs

showWrite :: Write a -> IO ()
showWrite = showWriteT . lift



newtype WriteTableField = WriteTableField { runWriteTableField :: VUM.IOVector Int -> Write () }

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
            EQ -> (Just y :*: branch)



newtype Write a = Write { unsafeRunWrite :: StateT Buffer IO a }
  deriving newtype MonadIO

instance Functor Write where
  {-# INLINE fmap #-}
  fmap f (Write run) = Write (fmap f run)

instance Applicative Write where
  {-# INLINE pure #-}
  pure a = Write $ pure a

  {-# INLINE (<*>) #-}
  fa <*> fb = Write $ unsafeRunWrite fa <*> unsafeRunWrite fb

instance Monad Write where
  {-# INLINE (>>=) #-}
  Write run >>= f = Write $ run >>= coerce f


--  $> import qualified FlatBuffers.Internal.Write as F

--  $> import qualified FlatBuffers.Internal.Read as F

--  $> :m +Control.Monad.Except

--  $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Int32 5 (\i -> pure (fromIntegral i * 3)) >> lift F.finish

--  $> let Right bs = F.runWrite F.defaultWriteSettings . runExceptT $ F.unfoldNM @Text 10 (\i -> pure $ T.replicate (i + 1) "a") >> lift F.finish

--  $> putStrLn $ F.showBuffer' bs



--  $> :m +Control.Monad.Except

--  $> runWrite defaultWriteSettings . runExceptT . fmap showBuffer' $ unfoldNM @Int32 4 (\i -> pure (fromIntegral @Int @Int32 $ i * 2)) >> lift finish

  -- $> F.showWrite $ do {
  -- $>   loc <- F.writeText "abc";
  -- $>   F.writeTable 2 (F.writeInt32 0 99 <> F.writeOffset 1 loc);
  -- $> }





class WriteVector a where
  type WriteVectorElem a

  unfoldNM :: (MonadTrans t, Monad (t Write)) => Int -> (Int -> t Write a) -> t Write (Location [WriteVectorElem a])

-- TODO: don't do unfoldNM, use `WriteVectorElement` like table fields.


instance WriteVector (Location a) where
  type WriteVectorElem (Location a) = a

  {-# INLINE unfoldNM #-}
  unfoldNM :: forall t. MonadTrans t => Monad (t Write) => Int -> (Int -> t Write (Location a)) -> t Write (Location [a])
  unfoldNM n f = do
    textLocations <- lift . liftIO $ VUM.new @IO @Int n

    forM_ [0 .. n - 1] $ \index -> do
      loc <- f index
      lift $ liftIO $ VUM.unsafeWrite textLocations index (getLocation loc)

    lift $ writeLocs textLocations
    lift getCurrentLocation


instance WriteVector Int32 where
  type WriteVectorElem Int32 = Int32

  {-# INLINE unfoldNM #-}
  unfoldNM :: forall t. (MonadTrans t, Monad (t Write)) => Int -> (Int -> t Write Int32) -> t Write (Location [Int32])
  unfoldNM n f = do
    let totalSize = 4 + 4 * n

    lift $ alignTo 4 totalSize
    sptr <- lift $ bufferSptr <$> getBuffer

    let sptr1 = sptr `minus` totalSize
    lift . liftIO $ putInt32 sptr1 (fromIntegral @Int @Int32 n)

    let sptr2 = sptr1 `plus` 4

    go (castPtr $ spPtr sptr2) 0

    lift $ modifyBuffer $ \b -> b { bufferSptr = sptr1 }

    lift getCurrentLocation

    where
      go :: Ptr Word8 -> Int -> t Write ()
      go ptr index
        | index >= n = pure ()
        | otherwise = do
            elem <- f index
            lift . liftIO $ putInt32 ptr elem
            go (ptr `plusPtr` 4) (index + 1)





writeLocs :: VUM.IOVector Int -> Write ()
writeLocs locs = do
  alignTo 4 (len * 4 + 4)
  go (len - 1)
  writeCount

  where
    len = VUM.length locs

    writeCount :: Write ()
    writeCount = do
      buffer <- getBuffer
      let sptr = bufferSptr buffer `minus` 4

      liftIO $ putInt32 sptr (fromIntegral @Int @Int32 len)

      putBuffer $ buffer { bufferSptr = sptr }

    go :: Int -> Write ()
    go index
      | index < 0 = pure ()
      | otherwise = do
          buffer <- getBuffer

          let sptr1 = bufferSptr buffer
          let sptr2 = sptr1 `minus` 4

          let currentLoc = bufferSize buffer + 4

          liftIO $ do
            textLoc <- VUM.unsafeRead locs index
            putInt32 sptr2 (fromIntegral @Int @Int32 $ currentLoc - textLoc)

          putBuffer $ buffer { bufferSptr = sptr2 }
          go (index - 1)

{-# INLINE getCurrentLocation #-}
getCurrentLocation :: Write (Location a)
getCurrentLocation = Location . bufferSize <$> getBuffer

{-# INLINE getBuffer #-}
getBuffer :: Write Buffer
getBuffer = Write get

{-# INLINE putBuffer #-}
putBuffer :: Buffer -> Write ()
putBuffer b = Write $ put b

modifyBuffer :: (Buffer -> Buffer) -> Write ()
modifyBuffer f = Write $ modify f

finish :: Write BS.ByteString
finish = do
  buffer <- getBuffer
  liftIO $ touchForeignPtr (bufferForeignPtr buffer)

  let offset = bufferCapacity buffer - bufferSize buffer
  pure $ BSI.PS (bufferForeignPtr buffer) offset (bufferSize buffer)

{-# INLINE finishWrite #-}
finishWrite :: WriteSettings -> Write a -> BS.ByteString
finishWrite ws w = runWrite ws $ w >> finish

{-# INLINE runWrite #-}
runWrite :: WriteSettings -> Write a -> a
runWrite (WriteSettings initialCapacity) write = unsafePerformIO $ do
  fp <- BSI.mallocByteString initialCapacity
  let ptr = SmartPtr (unsafeForeignPtrToPtr fp `plusPtr` initialCapacity) 0

  let initialBuffer = Buffer fp ptr initialCapacity (Max 1) M.empty

  evalStateT (unsafeRunWrite write) initialBuffer


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


{-# INLINE reserve #-}
reserve :: Int -> Write ()
reserve bytes = Write $ do
  buffer@(Buffer fp sptr capacity _ _) <- get
  let size = bufferSize buffer
  if capacity >= size + bytes
    then pure ()
    else do
      -- TODO: CHECK FOR INT32 OVERFLOWS
      -- (maybe cast to an unbound Int, and then check if it's > maxBound @Int32)?
      let newCapacity = (capacity * 2) `max` (capacity + bytes)

      -- Allocate new buffer and copy over the contents of the previous buffer
      newFp <- liftIO $ BSI.mallocByteString newCapacity
      let newPtr = unsafeForeignPtrToPtr newFp `plusPtr` (newCapacity - size)
      liftIO $ BSI.memcpy newPtr (spPtr sptr) size

      -- Make sure the previous `ForeignPtr` lives at least up until this point,
      -- to avoid invalidating its `Ptr`.
      -- See: https://hackage.haskell.org/package/base-4.12.0.0/docs/Foreign-ForeignPtr-Unsafe.html#v:unsafeForeignPtrToPtr
      liftIO $ touchForeignPtr fp

      put buffer
            { bufferForeignPtr = newFp
            , bufferSptr = SmartPtr newPtr size
            , bufferCapacity = newCapacity
            }

-- | Reserves at least @additionalBytes@ bytes and adds enough 0-padding so
-- that the buffer becomes aligned to @n@ after writing @additionalBytes@.
{-# INLINE alignTo #-}
alignTo :: Alignment{- ^ n -} -> Int {- ^ additionalBytes -} -> Write ()
alignTo !n !additionalBytes = do
  bsize <- bufferSize <$> getBuffer
  let padding = calcPadding n additionalBytes bsize
  reserve (padding + additionalBytes)
  if padding == 0
    then
      modifyBuffer $ \b -> b { bufferMaxAlign = bufferMaxAlign b <> Max n }
    else do
      buffer <- getBuffer
      let newSptr = bufferSptr buffer `minus` padding
      _ <- liftIO $ BSI.memset (spPtr newSptr) 0 (fromIntegral @Int @CSize padding)

      putBuffer buffer
        { bufferSptr = newSptr
        , bufferMaxAlign = bufferMaxAlign buffer <> Max n
        }

  -- | Calculate how much 0-padding is needed so that, after writing @additionalBytes@,
-- the buffer becomes aligned to @n@ bytes.
{-# INLINE calcPadding #-}
calcPadding :: Alignment {- ^ n -} -> Int {- ^ additionalBytes -} -> Int -> Int
calcPadding !n !additionalBytes bufferSize =
  (complement (bufferSize + additionalBytes) + 1) .&. (fromIntegral n - 1)

class Put p where
  putInt16 :: p -> Int16 -> IO ()
  putInt32 :: p -> Int32 -> IO ()
  putWord16 :: p -> Word16 -> IO ()
  putWord32 :: p -> Word32 -> IO ()

instance Put (Ptr a) where
  {-# INLINE putInt16 #-}
  putInt16 :: Ptr a -> Int16 -> IO ()
#ifdef WORDS_BIGENDIAN
#else
  putInt16 = poke . castPtr
#endif

  {-# INLINE putInt32 #-}
  putInt32 :: Ptr a -> Int32 -> IO ()
#ifdef WORDS_BIGENDIAN
#else
  putInt32 = poke . castPtr
#endif

  {-# INLINE putWord16 #-}
  putWord16 :: Ptr a -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
#else
  putWord16 = poke . castPtr
#endif


  {-# INLINE putWord32 #-}
  putWord32 :: Ptr a -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
#else
  putWord32 = poke . castPtr
#endif


instance Put SmartPtr where
  {-# INLINE putInt16 #-}
  {-# INLINE putInt32 #-}
  {-# INLINE putWord16 #-}
  {-# INLINE putWord32 #-}
  putInt16 = putInt16 . spPtr
  putInt32 = putInt32 . spPtr
  putWord16 = putWord16 . spPtr
  putWord32 = putWord32 . spPtr

writeText :: Text -> Write (Location Text)
writeText text@(TI.Text arr off len) = do
  bsize <- bufferSize <$> getBuffer
  utf8len <- liftIO $ fromIntegral @Int32 @Int <$> utf8length text
  let utf8lenAndTerminator = utf8len + 1
  let pad = calcPadding int32Size utf8lenAndTerminator bsize
  let padAndTerminator = pad + 1
  let totalBytes = int32Size + utf8lenAndTerminator + pad

  reserve totalBytes
  buffer <- getBuffer

  newSptr <- liftIO $ do
    let sptr1 = bufferSptr buffer

    let sptr2 = sptr1 `minus` padAndTerminator
    _ <- BSI.memset (spPtr sptr2) 0 (fromIntegral padAndTerminator)

    let sptr3 = sptr2 `minus` utf8len
    with (spPtr sptr3) $ \(destPtr :: Ptr (Ptr Word8)) ->
      c_encode_utf8 destPtr (A.aBA arr) (fromIntegral off) (fromIntegral len)

    let sptr4 = sptr3 `minus` int32Size
    putInt32 sptr4 (fromIntegral @Int @Int32 utf8len)

    pure sptr4

  let newBuffer = buffer
        { bufferSptr = newSptr
        , bufferMaxAlign = bufferMaxAlign buffer <> Max int32Size
        }
  putBuffer newBuffer

  pure (Location (bufferSize newBuffer))


newtype Location a = Location { getLocation :: Int }
  deriving newtype (Eq, Show)






{-# INLINE utf8length #-}
utf8length :: Text -> IO Int32
utf8length (TI.Text arr off len)
  | len == 0  = pure 0
  | otherwise = c_length_utf8 (A.aBA arr) (fromIntegral off) (fromIntegral len)

foreign import ccall unsafe "_hs_text_encode_utf8" c_encode_utf8
    :: Ptr (Ptr Word8) -> ByteArray# -> CSize -> CSize -> IO ()

foreign import ccall unsafe "_hs_text_length_utf8" c_length_utf8
  :: ByteArray# -> CSize -> CSize -> IO Int32




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