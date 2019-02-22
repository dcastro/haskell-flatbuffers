{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.Compiler.ParserSpec where

import           Data.List.NonEmpty
import           Data.Void                   (Void)
import           FlatBuffers.Compiler.Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.RawString.QQ           (r)

spec :: Spec
spec =
  fdescribe "Parser" $ do
    describe "include" $ do
      it "parses correctly" $
        parseEof include [r|include "abc";|] `shouldParse` "abc"
      it "parses strings with semicolons" $
        parseEof include [r|include "abc;";|] `shouldParse` "abc;"
      it "parses escaped strings" $ do
        parseEof include [r|include "abc \" " ;|] `shouldParse` "abc \" "
        parseEof include [r|include "abc \" escaped \" rest" ;|] `shouldParse` "abc \" escaped \" rest"
      describe "fails to parse" $ do
        it "unmatched quotes" $ 
          parseEof include "include \"abc;" `shouldFailWithError` "unexpected end of input\nexpecting '\"' or literal character\n"
        it "more than one string constant" $
          parseEof include "include \"abc\" \"def\";" `shouldFailWithError` "unexpected '\"'\nexpecting ';'\n"
        it "if there's no semicolon" $
          parseEof include "include \"abc\"" `shouldFailWithError` "unexpected end of input\nexpecting ';'\n"
    describe "schema" $ do
      it "empty schema" $
        [r||] `parses` Schema [] []
      it "with includes" $
        [r|
          include "somefile";
          include "otherFile";
        |] `parses` Schema ["somefile", "otherFile"] []
      it "with namespaces" $
        [r|
          include "somefile";
          namespace My.Api.Domain;
          namespace My.Api.Domain2;
        |] `parses`
          Schema ["somefile"] []
      it "table declarations" $
        [r|
          table ATable {
            aField : bool;
          }
        |] `parses`
          Schema
            []
            [TypeDecl Table "ATable" (Field "aField" Tbool :| [])]

shouldFailWithError :: Show a => Either (ParseErrorBundle String Void) a -> String -> Expectation
shouldFailWithError p s =
  case p of
    Left (ParseErrorBundle (x :| []) _) -> parseErrorTextPretty x `shouldBe` s
    Left (ParseErrorBundle xs _)        -> fail $ "Expected one parsing error, but got more:\n" ++ show xs
    Right a                             -> fail $ "Expected parsing to fail, but succeeded with:\n" ++ show a

parseEof :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parseEof p = parse (p <* eof) ""

parses :: String -> Schema -> Expectation
parses input expectedSchema =
  case parse schema "" input of
    l@(Left _) -> l `shouldParse` expectedSchema
    Right result -> result `shouldBe` expectedSchema


