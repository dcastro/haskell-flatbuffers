{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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
        [r||] `parses` Schema [] [] []

      it "includes" $
        [r|
          include "somefile";
          include "otherFile";
        |] `parses` Schema ["somefile", "otherFile"] [] []

      it "namespaces" $
        [r|
          include "somefile";
          namespace My.Api.Domain;
          namespace My.Api.Domain2;
        |] `parses`
          Schema ["somefile"] [] []

      it "table declarations" $
        [r|
          table ATable {
            abc : bool;
            d : Ref = 123;
            e : [uint] = 99.2e9;
            f : [abc_];
          }
        |] `parses`
          Schema
            []
            [TypeDecl Table "ATable" Nothing $ fromList
              [ Field "abc" Tbool Nothing Nothing
              , Field "d" (Tident "Ref") (Just "123") Nothing
              , Field "e" (Tvector Tword32) (Just "99.2e9") Nothing
              , Field "f" (Tvector (Tident "abc_")) Nothing Nothing
              ]
            ]
            []

      it "table declarations with metadata" $
        [r|
          table ATable ( a , b : 99992873786287637862.298736756627897654e999999 , c : 3 , d : "attr" ) {
            abc : bool = 99 ( def ) ;
          }
        |] `parses`
          Schema
            []
            [ TypeDecl Table "ATable"
              (Just (Metadata $ fromList
                [ ("a", Nothing)
                , ("b", Just (ConstN "99992873786287637862.298736756627897654e999999"))
                , ("c", Just (ConstN "3"))
                , ("d", Just (ConstS "attr"))
                ]
              ))
              (pure (Field "abc" Tbool (Just "99") (Just (Metadata (pure ("def", Nothing))))))
            ]
            []

      it "enum declarations" $
        [r|
          enum Color : short {
            Red,
            Blue = 18446744073709551615,
            Gray = -18446744073709551615,
            Black
          }
        |] `parses`
          Schema
            []
            []
            [EnumDecl "Color" Tint16 $ fromList
              [ EnumValDecl "Red" Nothing
              , EnumValDecl "Blue" (Just 18446744073709551615)
              , EnumValDecl "Gray" (Just (-18446744073709551615))
              , EnumValDecl "Black" Nothing
              ]
            ]

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


