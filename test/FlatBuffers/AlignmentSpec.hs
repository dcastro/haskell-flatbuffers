{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module FlatBuffers.AlignmentSpec where

import           Control.Monad.State.Strict  ( runState )

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import           Data.Coerce
import           Data.Foldable               ( fold )
import           Data.Int
import qualified Data.List                   as List
import           Data.Monoid                 ( Sum(..) )
import           Data.Semigroup              ( Max(..) )

import           Examples

import           FlatBuffers.Internal.Debug
import           FlatBuffers.Internal.Write
import           FlatBuffers.Types           ( Alignment(..), InlineSize(..), IsStruct(..) )

import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           TestImports

spec :: Spec
spec =
  describe "alignment" $ do
    describe "int64 are aligned to 8 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 8 8 (writeInt64TableField maxBound)

      it "in vectors" $ require $
        prop_vectorAlignment 8 8 (maxBound @Int64)

    describe "int32 are aligned to 4 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 4 4 (writeInt32TableField maxBound)

      it "in vectors" $ require $
        prop_vectorAlignment 4 4 (maxBound @Int32)

    describe "int16 are aligned to 4 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 2 2 (writeInt16TableField maxBound)

      it "in vectors" $ require $
        prop_vectorAlignment 2 2 (maxBound @Int16)

    describe "int8 are aligned to 4 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 1 1 (writeInt8TableField maxBound)

      it "in vectors" $ require $
        prop_vectorAlignment 1 1 (maxBound @Int8)


    describe "structs are aligned to the specified alignment" $ do
      describe "in table fields" $ do
        it "Struct1" $ require $
          prop_inlineTableFieldAlignment (fromIntegral (structSizeOf @Struct1)) (structAlignmentOf @Struct1)
            (writeStructTableField (struct1 1 2 3))

        it "Struct2" $ require $
          prop_inlineTableFieldAlignment (fromIntegral (structSizeOf @Struct2)) (structAlignmentOf @Struct2)
            (writeStructTableField (struct2 9))

        it "Struct3" $ require $
          prop_inlineTableFieldAlignment (fromIntegral (structSizeOf @Struct3)) (structAlignmentOf @Struct3)
            (writeStructTableField (struct3 (struct2 99) 2 3))

        it "Struct4" $ require $
          prop_inlineTableFieldAlignment (fromIntegral (structSizeOf @Struct4)) (structAlignmentOf @Struct4)
            (writeStructTableField (struct4 (struct2 99) 11 22 True))

      describe "in vectors" $ do
        it "Struct1" $ require $
          prop_vectorAlignment (fromIntegral (structSizeOf @Struct1)) (structAlignmentOf @Struct1)
            (struct1 maxBound maxBound maxBound)

        it "Struct2" $ require $
          prop_vectorAlignment (fromIntegral (structSizeOf @Struct2)) (structAlignmentOf @Struct2)
            (struct2 maxBound)

        it "Struct3" $ require $
          prop_vectorAlignment (fromIntegral (structSizeOf @Struct3)) (structAlignmentOf @Struct3)
            (struct3 (struct2 maxBound) maxBound maxBound)

        it "Struct4" $ require $
          prop_vectorAlignment (fromIntegral (structSizeOf @Struct4)) (structAlignmentOf @Struct4)
            (struct4 (struct2 maxBound) maxBound maxBound True)



prop_inlineTableFieldAlignment :: Int32 -> Alignment -> WriteTableField -> Property
prop_inlineTableFieldAlignment size alignment tableField = property $ do
  state1 <- forAllWith printFBState genInitialState
  let (f, state2) = runState (unWriteTableField tableField) state1
  let state3 = f state2

  -- `maxAlign` is either the previous `maxAlign` or the alignment of the last thing we wrote
  -- to the buffer, whichever's greatest
  maxAlign state3 === Max alignment `max` maxAlign state1
  fromIntegral (BSL.length (B.toLazyByteString (builder state3))) === bufferSize state3

  -- At most (alignment - 1) bytes can be added to the buffer as padding
  let padding = coerce bufferSize state3 - coerce bufferSize state1 - size
  padding `isLessThan` fromIntegral alignment

  -- The buffer is aligned to `alignment` bytes
  getSum (bufferSize state3) `mod` fromIntegral alignment === 0


prop_vectorAlignment ::
     WriteVectorElement a
  => Coercible (WriteVector a) WriteTableField
  => Int32 -> Alignment -> a -> Property
prop_vectorAlignment elemSize elemAlignment sampleElem = property $ do
  state1 <- forAllWith printFBState genInitialState
  vectorLength <- forAll $ Gen.int (Range.linear 0 5)

  let vec = vector' (List.replicate vectorLength sampleElem)
  let (writeUOffset, state2) = runState (unWriteTableField (writeVectorTableField vec)) state1
  let vectorByteCount = 4 + elemSize * fromIntegral vectorLength

  -- `maxAlign` is the greatest of: the previous alignment, the vector's elements alignment,
  -- or the vector size prefix alignment
  maxAlign state2 === maximum [Max elemAlignment, maxAlign state1, 4]
  fromIntegral (BSL.length (B.toLazyByteString (builder state2))) === bufferSize state2

  -- At most `n` bytes can be added to the  buffer as padding,
  -- `n` being the biggest thing we're aligning to: the vector's elements or the size prefix.
  let padding = coerce bufferSize state2 - coerce bufferSize state1 - vectorByteCount
  padding `isLessThan` (fromIntegral elemAlignment `max` 4)

  -- The entire vector, with the size prefix, is aligned to 4 bytes
  getSum (bufferSize state2) `mod` 4 === 0
  -- The vector, without the size prefix, is aligned to `elemAlignment` bytes
  getSum (bufferSize state2 - 4) `mod` fromIntegral elemAlignment === 0


  -- Write UOffset
  let state3 = writeUOffset state1

  maxAlign state3 === 4 `max` maxAlign state1
  fromIntegral (BSL.length (B.toLazyByteString (builder state3))) === bufferSize state3

  -- At most 4 bytes can be added to the buffer as padding
  let padding = bufferSize state3 - bufferSize state1 - 4
  padding `isLessThan` 4

  -- The buffer is aligned to 4 bytes
  getSum (bufferSize state3) `mod` 4 === 0


isLessThan :: (HasCallStack, MonadTest m, Show a, Num a, Ord a) => a -> a -> m ()
isLessThan x upper = do
  diff x (>=) 0
  diff x (<) upper

genInitialState :: Gen FBState
genInitialState = do
  bytes    <- Gen.bytes (Range.linear 0 50)
  maxAlign <- Gen.element [1, 2, 4, 8, 16]
  pure $ FBState
    { builder = B.byteString bytes
    , bufferSize = fromIntegral $ BS.length bytes
    , maxAlign = maxAlign
    , cache = mempty
    }

printFBState :: FBState -> String
printFBState (FBState builder bufferSize maxAlign cache) =
  fold
    [ "FBState"
    , "\n  { builder = "
    , "\n[ " <> showBuffer (B.toLazyByteString builder) <> " ]"
    , "\n  , bufferSize = " <> show bufferSize
    , "\n  , maxAlign = " <> show maxAlign
    , "\n  , cache = " <> show cache
    , "\n  }"
    ]
