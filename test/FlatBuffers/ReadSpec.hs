{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module FlatBuffers.ReadSpec where

import           Control.Exception          ( evaluate )

import           Data.Functor               ( ($>) )
import           Data.Int
import qualified Data.Maybe                 as Maybe

import           Examples

import           FlatBuffers.Internal.Read
import           FlatBuffers.Internal.Write
import qualified FlatBuffers.Vector         as Vec

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
        "UTF8 decoding error (byte 255): Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"

    it "fails when required field is missing" $ do
      table <- evalRight $ decode @RequiredFields $ encode $ writeTable []
      requiredFieldsA table `shouldBeLeft` "Missing required table field: a"
      requiredFieldsB table `shouldBeLeft` "Missing required table field: b"
      requiredFieldsC table `shouldBeLeft` "Missing required table field: c"
      requiredFieldsE table `shouldBeLeft` "Missing required table field: e"

      table <- evalRight $ decode @VectorOfUnions $ encode $ writeTable []
      vectorOfUnionsXsReq table `shouldBeLeft` "Missing required table field: xsReq"

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
      table <- evalRight $ decode $ encode $ writeTable
        [ writeVectorWord8TableField $ Vec.empty
        , missing
        , missing
        , missing
        , writeVectorWord8TableField $ Vec.empty
        , missing
        ]
      vectorOfUnionsXs table `shouldBeLeft` "Union vector: 'type vector' found but 'value vector' is missing."
      vectorOfUnionsXsReq table `shouldBeLeft` "Union vector: 'type vector' found but 'value vector' is missing."

    it "throws when union type vector and union value vector have different sizes" $ do
      let typesVec = Vec.singleton 1
      let valuesVec = Vec.empty
      table <- evalRight $ decode $ encode $ writeTable
        [ writeVectorWord8TableField typesVec
        , writeVectorTableTableField valuesVec
        ]
      vec <- evalRightJust $ vectorOfUnionsXs table
      toList vec `shouldBeLeft` "Union vector: 'type vector' and 'value vector' do not have the same length."

    describe "returns `UnionUnknown` when union type is not recognized" $ do
      it "in union table fields" $ do
        let union = writeUnion 99 (writeTable [])
        table <- evalRight $ decode $ encode $ tableWithUnion union
        tableWithUnionUni table `shouldBeRightAndExpect` \case
          UnionUnknown n -> n `shouldBe` 99

      it "in union vectors" $ do
        let union = writeUnion 99 (writeTable [])

        result <- evalRight $ do
          table <- decode $ encode $ vectorOfUnions Nothing (Vec.singleton union)
          vec   <- vectorOfUnionsXsReq table
          vec `unsafeIndex` 0

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
            (Maybe.fromJust vec) `indexFn` ix


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

      let testLargeUnsafeIndex table getVector = do
            case getIndex table getVector Vec.unsafeIndex 100 of
              Right a -> evaluate a $> ()
              Left e -> evaluate e $> ()
            case getIndex table getVector Vec.unsafeIndex (-100) of
              Right a -> evaluate a $> ()
              Left e -> evaluate e $> ()

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
          testLargeUnsafeIndex table vectorsA
          testLargeUnsafeIndex table vectorsB
          testLargeUnsafeIndex table vectorsC
          testLargeUnsafeIndex table vectorsD
          testLargeUnsafeIndex table vectorsE
          testLargeUnsafeIndex table vectorsF
          testLargeUnsafeIndex table vectorsG
          testLargeUnsafeIndex table vectorsH
          testLargeUnsafeIndex table vectorsI
          testLargeUnsafeIndex table vectorsJ
          testLargeUnsafeIndex table vectorsK
          testLargeUnsafeIndex table vectorsL

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

      describe "of structs" $ do
        let Right table = decode $ encode $ vectorOfStructs
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)
              (Just Vec.empty)

        it "`unsafeIndex` does not throw when index is negative / too large" $ do
          testLargeUnsafeIndex table vectorOfStructsAs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfStructsAs

        it "`index` throws when index is too large" $
          testLargeIndex table vectorOfStructsAs

      describe "of tables" $ do
        let Right table = decode $ encode $ vectorOfTables
              (Just Vec.empty)

        it "`unsafeIndex` does not throw when index is negative / too large" $ do
          testLargeUnsafeIndex table vectorOfTablesXs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfTablesXs

        it "`index` throws when index is too large" $
          testLargeIndex table vectorOfTablesXs

      describe "of unions" $ do
        let Right table = decode $ encode $ vectorOfUnions
              (Just Vec.empty)
              Vec.empty

        it "`unsafeIndex` does not throw when index is negative / too large" $ do
          testLargeUnsafeIndex table vectorOfUnionsXs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfUnionsXs

        it "`index` throws when index is too large" $
          testLargeIndex table vectorOfUnionsXs
