{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module FlatBuffers.ReadSpec where

import           Control.Exception          (evaluate)

import           Data.Functor               (($>))
import           Data.Int
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as Text

import           Examples

import           FlatBuffers.Internal.Read
import           FlatBuffers.Internal.Write
import qualified FlatBuffers.Vector         as Vec

import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           TestImports


spec :: Spec
spec =
  describe "read" $ do
    it "fails when buffer is exhausted" $
      decode @() "" `shouldBeLeft` "not enough bytes"

    it "fails when decoding string with invalid UTF-8 bytes" $ do
      let text = Vec.singleton 255
      table <- evalRight $ decode $ encode $ writeTable
        [ missing, missing, missing, missing
        , missing, missing, missing, missing
        , missing, missing, missing
        , writeVectorWord8TableField text
        ]
      primitivesL table `shouldBeLeft`
        "UTF8 decoding error (byte 255): Data.Text.Encoding: Invalid UTF-8 stream"

    it "fails when required field is missing" $ do
      table <- evalRight $ decode @RequiredFields $ encode $ writeTable []
      requiredFieldsA table `shouldBeLeft` "Missing required table field: a"
      requiredFieldsB table `shouldBeLeft` "Missing required table field: b"
      requiredFieldsC table `shouldBeLeft` "Missing required table field: c"
      requiredFieldsE table `shouldBeLeft` "Missing required table field: e"
      requiredFieldsF table `shouldBeLeft` "Missing required table field: f"

    it "returns `UnionNone` when required union field is missing" $ do
      table <- evalRight $ decode @RequiredFields $ encode $ writeTable []
      requiredFieldsD table `shouldBeRightAndExpect` \case
        UnionNone -> pure ()

      table <- evalRight $ decode @RequiredFields $ encode $ writeTable [ missing ]
      requiredFieldsD table `shouldBeRightAndExpect` \case
        UnionNone -> pure ()

    it "throws when union type is present, but union value is missing" $ do
      table <- evalRight $ decode $ encode $ writeTable [ writeWord8TableField 1]
      tableWithUnionUni table `shouldBeLeft` "Union: 'union type' found but 'union value' is missing."

    it "throws when union type vector is present, but union value vector is missing" $ do
      table <- evalRight $ decode @VectorOfUnions $ encode $ writeTable
        [ writeVectorWord8TableField Vec.empty
        , missing
        ]
      vectorOfUnionsXs table `shouldBeLeft` "Union vector: 'type vector' found but 'value vector' is missing."

    describe "returns `UnionUnknown` when union type is not recognized" $ do
      it "in union table fields" $ do
        let union = writeUnion 99 (writeTable [])
        table <- evalRight $ decode $ encode $ tableWithUnion union
        tableWithUnionUni table `shouldBeRightAndExpect` \case
          UnionUnknown n -> n `shouldBe` 99

      it "in union vectors" $ do
        let union = writeUnion 99 (writeTable [])

        result <- evalRight $ do
          table <- decode $ encode $ vectorOfUnions $ Just $ Vec.singleton union
          vectorOfUnionsXs table >>= \case
            Just vec -> vec `unsafeIndex` 0

        case result of
          UnionUnknown n -> n `shouldBe` 99

    describe "vectors" $ do
      let getIndex :: Table b
                  -> (Table b -> Either ReadError (Maybe (Vector a)))
                  -> (Vector a -> Int32 -> Either ReadError a)
                  -> Int32
                  -> Either ReadError a
          getIndex table getVector indexFn ix = do
            vec <- getVector table
            Maybe.fromJust vec `indexFn` ix


      let testNegativeIndex table getVector =
            (case getIndex table getVector Vec.index (-1) of
              Right a -> evaluate a $> ()
              Left e  -> evaluate e $> ()
            ) `shouldThrow` errorCall "FlatBuffers.Internal.Read.index: negative index: -1"

      let testLargeIndex table getVector =
            (case getIndex table getVector Vec.index 98 of
              Right a -> evaluate a $> ()
              Left e  -> evaluate e $> ()
            ) `shouldThrow` errorCall "FlatBuffers.Internal.Read.index: index too large: 98"

      let testInvalidUnsafeIndex table getVector = do
            case getIndex table getVector Vec.unsafeIndex 100 of
              Right a -> evaluate a $> ()
              Left e  -> evaluate e $> ()
            case getIndex table getVector Vec.unsafeIndex (-100) of
              Right a -> evaluate a $> ()
              Left e  -> evaluate e $> ()

      describe "of primitives" $ do
        let Right table = decode $ encode $ vectors
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)

        it "`unsafeIndex` does not throw when index is negative / too large" $ do
          testInvalidUnsafeIndex table vectorsA
          testInvalidUnsafeIndex table vectorsB
          testInvalidUnsafeIndex table vectorsC
          testInvalidUnsafeIndex table vectorsD
          testInvalidUnsafeIndex table vectorsE
          testInvalidUnsafeIndex table vectorsF
          testInvalidUnsafeIndex table vectorsG
          testInvalidUnsafeIndex table vectorsH
          testInvalidUnsafeIndex table vectorsI
          testInvalidUnsafeIndex table vectorsJ
          testInvalidUnsafeIndex table vectorsK
          testInvalidUnsafeIndex table vectorsL

        it "`index` throws when index is negative" $ do
          testNegativeIndex table vectorsA
          testNegativeIndex table vectorsB
          testNegativeIndex table vectorsC
          testNegativeIndex table vectorsD
          testNegativeIndex table vectorsE
          testNegativeIndex table vectorsF
          testNegativeIndex table vectorsG
          testNegativeIndex table vectorsH
          testNegativeIndex table vectorsI
          testNegativeIndex table vectorsJ
          testNegativeIndex table vectorsK
          testNegativeIndex table vectorsL

        it "`index` throws when index is too large" $ do
          testLargeIndex table vectorsA
          testLargeIndex table vectorsB
          testLargeIndex table vectorsC
          testLargeIndex table vectorsD
          testLargeIndex table vectorsE
          testLargeIndex table vectorsF
          testLargeIndex table vectorsG
          testLargeIndex table vectorsH
          testLargeIndex table vectorsI
          testLargeIndex table vectorsJ
          testLargeIndex table vectorsK
          testLargeIndex table vectorsL

        it "`take` and `drop` are consistent with Data.List.take and Data.List.drop" $
          requireProperty $ do
            listWord8  <- forAll $ Gen.list (Range.linear 0 20) (Gen.word8  (Range.linear 0 20))
            listWord16 <- forAll $ Gen.list (Range.linear 0 20) (Gen.word16 (Range.linear 0 20))
            listWord32 <- forAll $ Gen.list (Range.linear 0 20) (Gen.word32 (Range.linear 0 20))
            listWord64 <- forAll $ Gen.list (Range.linear 0 20) (Gen.word64 (Range.linear 0 20))
            listInt8   <- forAll $ Gen.list (Range.linear 0 20) (Gen.int8   (Range.linear -20 20))
            listInt16  <- forAll $ Gen.list (Range.linear 0 20) (Gen.int16  (Range.linear -20 20))
            listInt32  <- forAll $ Gen.list (Range.linear 0 20) (Gen.int32  (Range.linear -20 20))
            listInt64  <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64  (Range.linear -20 20))
            listFloat  <- forAll $ Gen.list (Range.linear 0 20) (Gen.float  (Range.linearFrac -20 20))
            listDouble <- forAll $ Gen.list (Range.linear 0 20) (Gen.double (Range.linearFrac -20 20))
            listBool   <- forAll $ Gen.list (Range.linear 0 20) Gen.bool
            listText   <- forAll $ Gen.list (Range.linear 0 20) (Gen.text (Range.singleton 3) Gen.alpha)

            n <- forAll $ Gen.int32 (Range.linearFrom 0 -10 30)

            table <- evalEither $ decode $ encode $ vectors
              (Just (Vec.fromList' listWord8))
              (Just (Vec.fromList' listWord16))
              (Just (Vec.fromList' listWord32))
              (Just (Vec.fromList' listWord64))
              (Just (Vec.fromList' listInt8))
              (Just (Vec.fromList' listInt16))
              (Just (Vec.fromList' listInt32))
              (Just (Vec.fromList' listInt64))
              (Just (Vec.fromList' listFloat))
              (Just (Vec.fromList' listDouble))
              (Just (Vec.fromList' listBool))
              (Just (Vec.fromList' listText))

            prop_takeConsistency n listWord8  (vectorsA table) pure
            prop_takeConsistency n listWord16 (vectorsB table) pure
            prop_takeConsistency n listWord32 (vectorsC table) pure
            prop_takeConsistency n listWord64 (vectorsD table) pure
            prop_takeConsistency n listInt8   (vectorsE table) pure
            prop_takeConsistency n listInt16  (vectorsF table) pure
            prop_takeConsistency n listInt32  (vectorsG table) pure
            prop_takeConsistency n listInt64  (vectorsH table) pure
            prop_takeConsistency n listFloat  (vectorsI table) pure
            prop_takeConsistency n listDouble (vectorsJ table) pure
            prop_takeConsistency n listBool   (vectorsK table) pure
            prop_takeConsistency n listText   (vectorsL table) pure

            prop_dropConsistency n listWord8  (vectorsA table) pure
            prop_dropConsistency n listWord16 (vectorsB table) pure
            prop_dropConsistency n listWord32 (vectorsC table) pure
            prop_dropConsistency n listWord64 (vectorsD table) pure
            prop_dropConsistency n listInt8   (vectorsE table) pure
            prop_dropConsistency n listInt16  (vectorsF table) pure
            prop_dropConsistency n listInt32  (vectorsG table) pure
            prop_dropConsistency n listInt64  (vectorsH table) pure
            prop_dropConsistency n listFloat  (vectorsI table) pure
            prop_dropConsistency n listDouble (vectorsJ table) pure
            prop_dropConsistency n listBool   (vectorsK table) pure
            prop_dropConsistency n listText   (vectorsL table) pure


      describe "of structs" $ do
        let Right table = decode $ encode $ vectorOfStructs
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)

        it "`unsafeIndex` does not throw when index is negative / too large" $
          testInvalidUnsafeIndex table vectorOfStructsAs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfStructsAs

        it "`index` throws when index is too large" $
          testLargeIndex table vectorOfStructsAs

        it "`take` and `drop` are consistent with Data.List.take and Data.List.drop" $
          requireProperty $ do
            listInt16 <- forAll $ Gen.list (Range.linear 0 20) (Gen.int16 (Range.linear -20 20))
            n <- forAll $ Gen.int32 (Range.linearFrom 0 -10 30)

            table <- evalEither $ decode $ encode $ vectorOfStructs
              Nothing
              (Just (Vec.fromList' (struct2 <$> listInt16)))
              Nothing
              Nothing

            prop_takeConsistency n listInt16 (vectorOfStructsBs table) struct2X
            prop_dropConsistency n listInt16 (vectorOfStructsBs table) struct2X

      describe "of tables" $ do
        let Right table = decode $ encode $ vectorOfTables
              (Just Vec.empty)

        it "`unsafeIndex` does not throw when index is negative / too large" $
          testInvalidUnsafeIndex table vectorOfTablesXs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfTablesXs

        it "`index` throws when index is too large" $
          testLargeIndex table vectorOfTablesXs

        it "`take` and `drop` are consistent with Data.List.take and Data.List.drop" $
          requireProperty $ do
            listInt32 <- forAll $ Gen.list (Range.linear 0 20) (Gen.int32 (Range.linear -20 20))
            n <- forAll $ Gen.int32 (Range.linearFrom 0 -10 30)

            table <- evalEither $ decode $ encode $ vectorOfTables
              (Just (Vec.fromList' (axe . Just <$> listInt32)))

            prop_takeConsistency n listInt32 (vectorOfTablesXs table) axeY
            prop_dropConsistency n listInt32 (vectorOfTablesXs table) axeY

      describe "of unions" $ do
        let Right table = decode $ encode $ vectorOfUnions
              (Just Vec.empty)

        it "`unsafeIndex` does not throw when index is negative / too large" $
          testInvalidUnsafeIndex table vectorOfUnionsXs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfUnionsXs

        it "`index` throws when index is too large" $
          testLargeIndex table vectorOfUnionsXs

        it "`take` and `drop` are consistent with Data.List.take and Data.List.drop" $
          requireProperty $ do
            listOfPairs :: [(String, Int32)] <- forAll $ Gen.list (Range.linear 0 20) $ do
              unionType <- Gen.element ["Axe", "Sword"]
              unionVal <- Gen.int32 (Range.linear -20 20)
              pure (unionType, unionVal)

            n <- forAll $ Gen.int32 (Range.linearFrom 0 -10 30)

            let pairToUnion :: (String, Int32) -> WriteUnion Weapon
                pairToUnion = \case
                  ("Axe", val)   -> weaponAxe (axe (Just val))
                  ("Sword", val) -> weaponSword (sword (Just (Text.pack (show val))))

            let unionToPair :: Union Weapon -> Either ReadError (String, Int32)
                unionToPair = \case
                  Union (WeaponAxe axe) -> do
                    val <- axeY axe
                    pure ("Axe", val)
                  Union (WeaponSword sword) -> do
                    textValMaybe <- swordX sword
                    case textValMaybe of
                      Just textVal ->
                        case Text.signed Text.decimal textVal of
                          Right (intVal, _) ->
                            pure ("Sword", intVal)

            table <- evalEither $ decode $ encode $ vectorOfUnions $ Just $
              Vec.fromList' (pairToUnion <$> listOfPairs)

            prop_takeConsistency n listOfPairs (vectorOfUnionsXs table) unionToPair
            prop_dropConsistency n listOfPairs (vectorOfUnionsXs table) unionToPair


prop_takeConsistency ::
  (Eq a, Show a, VectorElement b)
  => Int32
  -> [a]
  -> Either ReadError (Maybe (Vector b))
  -> (b -> Either ReadError a)
  -> PropertyT IO ()
prop_takeConsistency n list vec extract = do
  Just vec <- evalEither vec
  (Vec.toList (Vec.take n vec) >>= traverse extract) === Right (List.take (fromIntegral n) list)
  Vec.length (Vec.take n vec) === fromIntegral (List.length (List.take (fromIntegral n) list))

prop_dropConsistency ::
  (Eq a, Show a, VectorElement b)
  => Int32
  -> [a]
  -> Either ReadError (Maybe (Vector b))
  -> (b -> Either ReadError a)
  -> PropertyT IO ()
prop_dropConsistency n list vec extract = do
  Just vec <- evalEither vec
  (Vec.toList (Vec.drop n vec) >>= traverse extract) === Right (List.drop (fromIntegral n) list)
  Vec.length (Vec.drop n vec) === fromIntegral (List.length (List.drop (fromIntegral n) list))
