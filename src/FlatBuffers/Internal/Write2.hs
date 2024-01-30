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

module FlatBuffers.Internal.Write2 where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST (runST)
import Control.Monad.State.Strict as S
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Int
import Data.List qualified as List
import Data.Map.Internal qualified as MI
import Data.Map.Strict qualified as M
import Data.Map.Strict.Internal qualified as MSI
import Data.Semigroup (Max(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as A
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Lazy qualified as LT
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word
import Debug.Trace
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

 -- $> putStrLn $ F.showBuffer' bs

{-


>>> import qualified FlatBuffers.Internal.Write2 as F

>>> let enc = prettyPrint . showBuffer' . F.encodeDef


-- >>> F.showWrite2 do { loc <- F.writeText "abc"; F.writeTable 2 (F.writeInt32TableField 0 99 <> F.writeOffsetTableField 1 loc); }
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

>>> enc do { loc <- F.writeText "abc"; F.writeTable 2 (F.writeInt32TableField 0 99 <> F.writeOffsetTableField 1 loc); }
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


{-# INLINE writeInt32TableField #-}
writeInt32TableField :: Int -> Int32 -> WriteTableField
writeInt32TableField fieldIndex i = WriteTableField $ \locs -> do
  alignTo 4 4
  unsafeWriteInt32 i
  buffer <- getBuffer
  liftIO $ VUM.unsafeWrite locs fieldIndex buffer.bufferSptr.spOffset

-- | This function is unsafe because it may potentially write outside the buffer's boundaries.
-- Make sure to use `reserve` (or `alignTo`) before using this function.
unsafeWriteInt32 :: Int32 -> Write ()
unsafeWriteInt32 i = do
  buffer <- getBuffer
  let sptr = buffer.bufferSptr `minus` int32Size
  liftIO $ putInt32 sptr i
  putBuffer buffer { bufferSptr = sptr}

writeOffsetTableField :: Int -> Location a -> WriteTableField
writeOffsetTableField fieldIndex loc = WriteTableField $ \locs -> do
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

showWrite2 :: Write a -> PrettyString
showWrite2 w = prettyPrint $ showBuffer' $ runWrite defaultWriteSettings $ w >> finish


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
            EQ -> Just y :*: branch



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
  -- $>   F.writeTable 2 (F.writeInt32TableField 0 99 <> F.writeOffsetTableField 1 loc);
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

writeUOffsetFrom :: Location a -> Write ()
writeUOffsetFrom loc = do
  alignTo uoffsetSize 0
  buffer <- getBuffer
  let currentLoc = bufferSize buffer
  let uoffset = fromIntegral @Int @Int32 $ currentLoc - loc.getLocation + uoffsetSize
  unsafeWriteInt32 uoffset

encode :: WriteSettings -> Write (Location a) -> BS.ByteString
encode settings writeTable =
  runWrite settings do
    writeTableRoot
    finish
  where
    writeTableRoot = do
      tableRoot <- writeTable
      maxAlignment <- gets $ getMax . bufferMaxAlign
      alignTo maxAlignment uoffsetSize
      writeUOffsetFrom tableRoot

encodeDef :: Write (Location a) -> BS.ByteString
encodeDef = encode defaultWriteSettings

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
      liftIO $ Marshal.copyBytes newPtr (spPtr sptr) size

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
      _ <- liftIO $ Marshal.fillBytes (spPtr newSptr) 0 padding

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
  -- TODO
#else
  putInt16 = poke . castPtr
#endif

  {-# INLINE putInt32 #-}
  putInt32 :: Ptr a -> Int32 -> IO ()
#ifdef WORDS_BIGENDIAN
  -- TODO
#else
  putInt32 = poke . castPtr
#endif

  {-# INLINE putWord16 #-}
  putWord16 :: Ptr a -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
  -- TODO
#else
  putWord16 = poke . castPtr
#endif


  {-# INLINE putWord32 #-}
  putWord32 :: Ptr a -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
  -- TODO
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
  let utf8len = utf8length text
  let utf8lenAndTerminator = utf8len + 1
  let pad = calcPadding int32Size utf8lenAndTerminator bsize
  let padAndTerminator = pad + 1
  let totalBytes = int32Size + utf8lenAndTerminator + pad

  reserve totalBytes
  buffer <- getBuffer

  newSptr <- liftIO $ do
    let sptr1 = bufferSptr buffer

    let sptr2 = sptr1 `minus` padAndTerminator
    Marshal.fillBytes (spPtr sptr2) 0 (fromIntegral padAndTerminator)

    let sptr3 = sptr2 `minus` utf8len
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


newtype Location a = Location { getLocation :: Int }
  deriving newtype (Eq, Show)

{-# INLINE utf8length #-}
utf8length :: Text -> Int
utf8length (TI.Text _array _offset len) = len

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
