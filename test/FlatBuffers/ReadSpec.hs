{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module FlatBuffers.ReadSpec where

import           Control.Exception          ( evaluate )

import qualified Data.ByteString.Lazy       as BSL
import           Data.Functor               ( ($>) )
import qualified Data.Maybe                 as Maybe
import           Data.Word

import           Examples

import           FlatBuffers.Internal.Read
import           FlatBuffers.Internal.Write

import           TestImports

spec :: Spec
spec =
  describe "read" $ do
    it "fails when buffer is exhausted" $
      decode @() "" `shouldBeLeft` ParsingError "not enough bytes"

    it "fails when decoding string with invalid UTF-8 bytes" $ do
      let text = vector' @Word8 [ 255 ]
      table <- evalRight $ decode $ encode $ writeTable
        [ missing, missing, missing, missing
        , missing, missing, missing, missing
        , missing, missing, missing
        , writeVectorWord8TableField text
        ]
      primitivesL table `shouldBeLeft`
        Utf8DecodingError "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" (Just 255)

    it "fails when required field is missing" $ do
      table <- evalRight $ decode @RequiredFields $ encode $ writeTable []
      requiredFieldsA table `shouldBeLeft` MissingField "a"
      requiredFieldsB table `shouldBeLeft` MissingField "b"
      requiredFieldsC table `shouldBeLeft` MissingField "c"
      requiredFieldsE table `shouldBeLeft` MissingField "e"

      table <- evalRight $ decode @VectorOfUnions $ encode $ writeTable []
      vectorOfUnionsXsReq table `shouldBeLeft` MissingField "xsReq"

    it "returns `UnionNone` when required union field is missing" $ do
      table <- evalRight $ decode @RequiredFields $ encode $ writeTable []
      requiredFieldsD table `shouldBeRightAndExpect` \case
        UnionNone -> pure ()

      table <- evalRight $ decode @RequiredFields $ encode $ writeTable [ missing ]
      requiredFieldsD table `shouldBeRightAndExpect` \case
        UnionNone -> pure ()

    it "throws when union type is present, but union value is missing" $ do
      table <- evalRight $ decode $ encode $ writeTable [ writeWord8TableField 1]
      tableWithUnionUni table `shouldBeLeft` MalformedBuffer "Union: 'union type' found but 'union value' is missing."

    it "throws when union type vector is present, but union value vector is missing" $ do
      table <- evalRight $ decode $ encode $ writeTable
        [ writeVectorWord8TableField $ vector' @Word8 []
        , missing
        , missing
        , missing
        , writeVectorWord8TableField $ vector' @Word8 []
        , missing
        ]
      vectorOfUnionsXs table `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."
      vectorOfUnionsXsReq table `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' found but 'value vector' is missing."

    it "throws when union type vector and union value vector have different sizes" $ do
      let typesVec = vector' [ 1 ]
      let valuesVec = vector' []
      table <- evalRight $ decode $ encode $ writeTable
        [ writeVectorWord8TableField typesVec
        , writeVectorTableTableField valuesVec
        ]
      vec <- evalRightJust $ vectorOfUnionsXs table
      toList vec `shouldBeLeft` MalformedBuffer "Union vector: 'type vector' and 'value vector' do not have the same length."

    describe "returns `UnionUnknown` when union type is not recognized" $ do
      it "in union table fields" $ do
        let union = writeUnion 99 (writeTable [])
        table <- evalRight $ decode $ encode $ tableWithUnion union
        tableWithUnionUni table `shouldBeRightAndExpect` \case
          UnionUnknown n -> n `shouldBe` 99

      it "in union vectors" $ do
        let union = writeUnion 99 (writeTable [])

        result <- evalRight $ do
          table <- decode $ encode $ vectorOfUnions Nothing (vector' [union])
          vec   <- vectorOfUnionsXsReq table
          vec `index` 0

        case result of
          UnionUnknown n -> n `shouldBe` 99

    describe "vectors" $ do
      let getIndex table getVector ix = do
            vec <- getVector table
            (Maybe.fromJust vec) `index` ix

      let testInvalidIndex (Right table) getVector =
            getIndex table getVector 100
              `shouldBeLeft` ParsingError "not enough bytes"

      let testNegativeIndex (Right table) getVector =
            (case getIndex table getVector (-1) of
              Right a -> evaluate a $> ()
              Left e  -> evaluate e $> ()
            ) `shouldThrow` errorCall "FlatBuffers.Internal.Read.index: negative index: -1"

      describe "of primitives" $ do
        let table = decode $ encode $ vectors
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))

        it "`index` fails when index points to a location beyond the buffer's limits" $ do
          testInvalidIndex table vectorsA
          testInvalidIndex table vectorsB
          testInvalidIndex table vectorsC
          testInvalidIndex table vectorsD
          testInvalidIndex table vectorsE
          testInvalidIndex table vectorsF
          testInvalidIndex table vectorsG
          testInvalidIndex table vectorsH
          testInvalidIndex table vectorsI
          testInvalidIndex table vectorsJ
          testInvalidIndex table vectorsK
          testInvalidIndex table vectorsL

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

      describe "of structs" $ do
        let table = decode $ encode $ vectorOfStructs
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))
              (Just (vector' []))

        it "`index` returns struct pointing to empty string when index points to a location beyond the buffer's limits" $ do
          table <- evalRight table
          vec <- evalRightJust $ vectorOfStructsAs table
          Struct bs <- evalRight $ vec `index` 100
          BSL.null bs `shouldBe` True

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfStructsAs

      describe "of tables" $ do
        let table = decode $ encode $ vectorOfTables
              (Just (vector' []))

        it "`index` fails when index points to a location beyond the buffer's limits" $ do
          testInvalidIndex table vectorOfTablesXs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfTablesXs

      describe "of unions" $ do
        let table = decode $ encode $ vectorOfUnions
              (Just (vector' []))
              (vector' [])

        it "`index` fails when index points to a location beyond the buffer's limits" $ do
          testInvalidIndex table vectorOfUnionsXs

        it "`index` throws when index is negative" $
          testNegativeIndex table vectorOfUnionsXs
