{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlatBuffers.AlignmentSpec where

import           Control.Monad.State.Strict

import qualified Data.Binary.Get            as G
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as BSL
import           Data.ByteString.Lazy       ( ByteString )
import           Data.Coerce
import           Data.Foldable              ( fold, foldrM )
import           Data.Int
import qualified Data.List                  as List
import           Data.Monoid                ( Sum(..) )
import           Data.Semigroup             ( Max(..) )
import           Data.Text                  ( Text )
import qualified Data.Text.Encoding         as T
import           Data.Word

import           Examples

import           FlatBuffers.Internal.Debug
import           FlatBuffers.Internal.Write
import           FlatBuffers.Types          ( Alignment(..), InlineSize(..), IsStruct(..) )

import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           TestImports


spec :: Spec
spec =
  describe "alignment" $ do
    describe "Int8 are aligned to 1 byte" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 1 1 (writeInt8TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 1 1 (maxBound @Int8)

    describe "Int16 are aligned to 2 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 2 2 (writeInt16TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 2 2 (maxBound @Int16)

    describe "Int32 are aligned to 4 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 4 4 (writeInt32TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 4 4 (maxBound @Int32)

    describe "Int64 are aligned to 8 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 8 8 (writeInt64TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 8 8 (maxBound @Int64)

    describe "Word8 are aligned to 1 byte" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 1 1 (writeWord8TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 1 1 (maxBound @Word8)

    describe "Word16 are aligned to 2 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 2 2 (writeWord16TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 2 2 (maxBound @Word16)

    describe "Word32 are aligned to 4 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 4 4 (writeWord32TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 4 4 (maxBound @Word32)

    describe "Word64 are aligned to 8 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 8 8 (writeWord64TableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 8 8 (maxBound @Word64)

    describe "Float are aligned to 4 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 4 4 (writeFloatTableField 999.5)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 4 4 (999.5 :: Float)

    describe "Double are aligned to 8 bytes" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 8 8 (writeDoubleTableField 999.5)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 8 8 (999.5 :: Double)

    describe "Bool are aligned to 1 byte" $ do
      it "in table fields" $ require $
        prop_inlineTableFieldAlignment 1 1 (writeBoolTableField maxBound)
      it "in vectors" $ require $
        prop_inlineVectorAlignment 1 1 (maxBound @Bool)


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
          prop_inlineVectorAlignment (fromIntegral (structSizeOf @Struct1)) (structAlignmentOf @Struct1)
            (struct1 maxBound maxBound maxBound)

        it "Struct2" $ require $
          prop_inlineVectorAlignment (fromIntegral (structSizeOf @Struct2)) (structAlignmentOf @Struct2)
            (struct2 maxBound)

        it "Struct3" $ require $
          prop_inlineVectorAlignment (fromIntegral (structSizeOf @Struct3)) (structAlignmentOf @Struct3)
            (struct3 (struct2 maxBound) maxBound maxBound)

        it "Struct4" $ require $
          prop_inlineVectorAlignment (fromIntegral (structSizeOf @Struct4)) (structAlignmentOf @Struct4)
            (struct4 (struct2 maxBound) maxBound maxBound True)

    describe "Text are aligned to 4 bytes" $ do
      it "in table fields" $ require prop_textTableFieldAlignment
      it "in vectors" $ require prop_textVectorAlignment



prop_inlineTableFieldAlignment :: Int32 -> Alignment -> WriteTableField -> Property
prop_inlineTableFieldAlignment size alignment tableField = property $ do
  initialState <- forAllWith printFBState genInitialState
  let (f, interimState) = runState (unWriteTableField tableField) initialState
  let finalState = f interimState

  testBufferSizeIntegrity finalState
  testMaxAlign initialState finalState alignment

  -- At most (alignment - 1) bytes can be added to the buffer as padding
  let padding = coerce bufferSize finalState - coerce bufferSize initialState - size
  padding `isLessThan` fromIntegral alignment

  -- The buffer is aligned to `alignment` bytes
  getSum (bufferSize finalState) `mod` fromIntegral alignment === 0


prop_inlineVectorAlignment ::
     WriteVectorElement a
  => Coercible (WriteVector a) WriteTableField
  => Int32 -> Alignment -> a -> Property
prop_inlineVectorAlignment elemSize elemAlignment sampleElem = property $ do
  initialState <- forAllWith printFBState genInitialState
  vectorLength <- forAll $ Gen.int (Range.linear 0 5)

  let vec = vector' (List.replicate vectorLength sampleElem)
  let (writeUOffset, finalState) = runState (unWriteTableField (writeVectorTableField vec)) initialState

  testBufferSizeIntegrity finalState
  testMaxAlign initialState finalState (elemAlignment `max` 4)
  testUOffsetAlignment writeUOffset

  -- The entire vector, with the size prefix, is aligned to 4 bytes
  bufferSize finalState `isAlignedTo` 4
  -- The vector, without the size prefix, is aligned to `elemAlignment` bytes
  (bufferSize finalState - 4) `isAlignedTo` fromIntegral elemAlignment

  -- At most `n` bytes can be added to the  buffer as padding,
  -- `n` being the biggest thing we're aligning to: the vector's elements or the size prefix.
  let vectorByteCount = 4 + elemSize * fromIntegral vectorLength
  let padding = coerce bufferSize finalState - coerce bufferSize initialState - vectorByteCount
  padding `isLessThan` (fromIntegral elemAlignment `max` 4)


prop_textTableFieldAlignment :: Property
prop_textTableFieldAlignment = property $ do
  initialState <- forAllWith printFBState genInitialState
  text <- forAll $ Gen.text (Range.linear 0 30) Gen.unicode

  let (writeUOffset, finalState) = runState (unWriteTableField (writeTextTableField text)) initialState

  testBufferSizeIntegrity finalState
  testMaxAlign initialState finalState 4
  testUOffsetAlignment writeUOffset
  bufferSize finalState `isAlignedTo` 4

  -- At most 4 bytes can be added to the buffer as padding
  let textByteCount = BS.length (T.encodeUtf8 text) + 1
  let padding = bufferSize finalState - bufferSize initialState - fromIntegral textByteCount - 4
  padding `isLessThan` 4


prop_textVectorAlignment :: Property
prop_textVectorAlignment = property $ do
  initialState <- forAllWith printFBState genInitialState
  texts <- forAll $ Gen.list (Range.linear 0 5) (Gen.text (Range.linear 0 30) Gen.unicode)

  let (writeUOffset, finalState) = runState (unWriteTableField (writeVectorTableField (vector' texts))) initialState

  testBufferSizeIntegrity finalState
  testMaxAlign initialState finalState 4
  testUOffsetAlignment writeUOffset
  bufferSize finalState `isAlignedTo` 4

  let initialBuffer = B.toLazyByteString (builder initialState)
  let finalBuffer = B.toLazyByteString (builder finalState)

  let jumpToTextAtIndex :: Int -> ByteString
      jumpToTextAtIndex index =
        flip G.runGet finalBuffer $ do
          G.skip (4 + index * 4)
          offset <- G.getInt32le
          G.skip (fromIntegral offset - 4)
          G.getRemainingLazyByteString

  let checkTextAlignment :: (Text, Int) -> ByteString -> PropertyT IO ByteString
      checkTextAlignment (text, index) previousBuffer = do
        let newBuffer = jumpToTextAtIndex index
        BSL.length newBuffer `isAlignedTo` 4

        let textByteCount = BS.length (T.encodeUtf8 text) + 1
        let padding = BSL.length newBuffer - BSL.length previousBuffer - fromIntegral textByteCount - 4
        padding `isLessThan` 4

        pure newBuffer

  -- Cycle through every text, right to left, see if it has been properly padded/aligned
  -- relative to the bytestring that follows it.
  -- When we're done, `bufferWithTexts` will point to the position where the texts begin.
  bufferWithTexts <- foldrM checkTextAlignment initialBuffer (texts `zip` [0..])

  -- At most 4 bytes can be added to the buffer as padding,
  -- between the vector of offsets and the texts.
  let padding = BSL.length finalBuffer - BSL.length bufferWithTexts - (4 + 4 * fromIntegral (List.length texts))
  padding `isLessThan` 4


testUOffsetAlignment :: (FBState -> FBState) -> PropertyT IO ()
testUOffsetAlignment writeUOffset = do
  initialState <- forAllWith printFBState genInitialState
  let finalState = writeUOffset initialState

  testBufferSizeIntegrity finalState
  testMaxAlign initialState finalState 4
  bufferSize finalState `isAlignedTo` 4

  -- At most 4 bytes can be added to the buffer as padding
  let padding = bufferSize finalState - bufferSize initialState - 4
  padding `isLessThan` 4


-- | `bufferSize` should always be equal to the size of the bytestring produced by the builder
testBufferSizeIntegrity :: FBState -> PropertyT IO ()
testBufferSizeIntegrity state =
  bufferSize state === fromIntegral (BSL.length (B.toLazyByteString (builder state)))

-- | `maxAlign` is either the previous `maxAlign` or the alignment of the last thing we wrote
-- to the buffer, whichever's greatest
testMaxAlign :: FBState -> FBState -> Alignment -> PropertyT IO ()
testMaxAlign initialState finalState alignment =
  maxAlign finalState === maxAlign initialState `max` coerce alignment

isAlignedTo :: Integral bufferSize => bufferSize -> Int32 -> PropertyT IO ()
isAlignedTo size alignment =
  fromIntegral size `mod` alignment === 0

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

deriving instance Real a => Real (Sum a)
deriving instance Enum a => Enum (Sum a)
deriving instance Integral a => Integral (Sum a)
