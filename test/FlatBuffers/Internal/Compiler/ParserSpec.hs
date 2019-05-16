{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FlatBuffers.Internal.Compiler.ParserSpec where

import           Data.Void                                ( Void )

import           FlatBuffers.Internal.Compiler.Parser
import           FlatBuffers.Internal.Compiler.SyntaxTree

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           TestUtils

import           Text.Megaparsec
import           Text.RawString.QQ                        ( r )

spec :: Spec
spec =
  describe "Parser" $ do
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

      it "includes" $
        [r|
          include "somefile";
          include "other \"escaped\" File";
        |] `parses` Schema ["somefile", "other \"escaped\" File"] []

      it "includes" $
        [r|
          include "a";
          native_include "b";
          include "c";
          native_include "d";
          include "e";
        |] `parses` Schema ["a", "c", "e"] []

      it "namespaces" $
        [r|
          include "somefile";
          namespace Ns;
          namespace My . Api . Domain;
          namespace My.Api.Domain2;
          namespace ;
        |] `parses`
          Schema
            ["somefile"]
            [ DeclN "Ns"
            , DeclN "My.Api.Domain"
            , DeclN "My.Api.Domain2"
            , DeclN ""
            ]

      it "table declarations" $
        [r|
          table T {}

          table ATable {
            abc : bool;
            b1 : bool = true;
            b2 : bool = false;
            d : Ref = 123;
            d : uint;
            d : uint_;
            d : X.uint;
            d : X.uint_;
            e : [uint] = - 99.2e9 ;
            e : [uint] = 99992873786287637862.298736756627897654e99 ;
            f : [uint_];
            g : My . Api . Ref = 123;
            h : [ MyApi.abc_ ] ;
            i: Color = Blue ;
          }
        |] `parses`
          Schema
            []
            [ DeclT $ TableDecl "T" (Metadata mempty) []
            , DeclT $ TableDecl "ATable" (Metadata mempty)
              [ TableField "abc" TBool Nothing (Metadata mempty)
              , TableField "b1" TBool (Just (DefaultBool True)) (Metadata mempty)
              , TableField "b2" TBool (Just (DefaultBool False)) (Metadata mempty)
              , TableField "d" (TRef (TypeRef "" "Ref")) (Just (DefaultNum 123)) (Metadata mempty)
              , TableField "d" TWord32 Nothing (Metadata mempty)
              , TableField "d" (TRef (TypeRef "" "uint_")) Nothing (Metadata mempty)
              , TableField "d" (TRef (TypeRef "X" "uint")) Nothing (Metadata mempty)
              , TableField "d" (TRef (TypeRef "X" "uint_")) Nothing (Metadata mempty)
              , TableField "e" (TVector TWord32) (Just (DefaultNum (-99.2e9))) (Metadata mempty)
              , TableField "e" (TVector TWord32) (Just (DefaultNum 99992873786287637862.298736756627897654e99)) (Metadata mempty)
              , TableField "f" (TVector (TRef (TypeRef "" "uint_"))) Nothing (Metadata mempty)
              , TableField "g" (TRef (TypeRef "My.Api" "Ref")) (Just (DefaultNum 123)) (Metadata mempty)
              , TableField "h" (TVector (TRef (TypeRef "MyApi" "abc_"))) Nothing (Metadata mempty)
              , TableField "i" (TRef (TypeRef "" "Color")) (Just (DefaultRef "Blue")) (Metadata mempty)
              ]
            ]

      it "struct declarations" $
        [r|
          struct AStruct {
            abc : bool;
            d : Ref ;
            e : [uint] ;
            f : [uint_];
            g : My . Api . Ref ;
            h : [ MyApi.abc_ ] ;
          }
        |] `parses`
          Schema
            []
            [ DeclS $ StructDecl "AStruct" (Metadata mempty)
              [ StructField "abc" TBool (Metadata mempty)
              , StructField "d" (TRef (TypeRef "" "Ref")) (Metadata mempty)
              , StructField "e" (TVector TWord32) (Metadata mempty)
              , StructField "f" (TVector (TRef (TypeRef "" "uint_"))) (Metadata mempty)
              , StructField "g" (TRef (TypeRef "My.Api" "Ref")) (Metadata mempty)
              , StructField "h" (TVector (TRef (TypeRef "MyApi" "abc_"))) (Metadata mempty)
              ]
            ]

      it "table declarations with metadata" $
        [r|
          table ATable ( a , "b" : 9283 , c : "attr" ) {
            abc : bool = 99 ( def ) ;
          }
        |] `parses`
          Schema
            []
            [ DeclT $ TableDecl "ATable"
              (Metadata
                [ ("a", Nothing)
                , ("b", Just (AttrI 9283))
                , ("c", Just (AttrS "attr"))
                ]
              )
              (pure (TableField "abc" TBool (Just (DefaultNum 99)) (Metadata [("def", Nothing)])))
            ]

      it "enum declarations" $
        [r|
          enum Color : short (attr) {
            Red,
            Blue = 18446744073709551615,
            Gray = -18446744073709551615,
            Black
          }
        |] `parses`
          Schema
            []
            [DeclE $ EnumDecl "Color" TInt16 (Metadata [("attr", Nothing)])
              [ EnumVal "Red" Nothing
              , EnumVal "Blue" (Just 18446744073709551615)
              , EnumVal "Gray" (Just (-18446744073709551615))
              , EnumVal "Black" Nothing
              ]
            ]

      it "union declarations" $
        [r|
          union Weapon ( attr ) {
            Sword,
            mace: Stick,
            mace2: My.Api.Stick,
            Axe
          }
        |] `parses`
          Schema
            []
            [ DeclU $ UnionDecl
                "Weapon"
                (Metadata [("attr", Nothing)])
                [ UnionVal Nothing (TypeRef "" "Sword")
                , UnionVal (Just "mace") (TypeRef "" "Stick")
                , UnionVal (Just "mace2") (TypeRef "My.Api" "Stick")
                , UnionVal Nothing (TypeRef "" "Axe")
                ]
            ]

      it "root types, file extensions / identifiers, attribute declarations" $
        [r|
          attribute a;
          attribute "b";
          root_type c;
          root_type My.Api.C ;
          file_extension "d";
          file_identifier "abcd";
        |] `parses`
          Schema
            []
            [ DeclA $ AttributeDecl "a"
            , DeclA $ AttributeDecl "b"
            , DeclR $ RootDecl (TypeRef "" "c")
            , DeclR $ RootDecl (TypeRef "My.Api" "C")
            , DeclFI $ FileIdentifierDecl "abcd"
            ]

      it "file identifier must have exactly 4 UTF-8 code units" $ do
        parseEof schema [r| file_identifier "";      |] `shouldFailWithError` "file_identifier must be exactly 4 characters\n"
        parseEof schema [r| file_identifier "abc";   |] `shouldFailWithError` "file_identifier must be exactly 4 characters\n"
        parseEof schema [r| file_identifier "abcde"; |] `shouldFailWithError` "file_identifier must be exactly 4 characters\n"
        parseEof schema [r| file_identifier "abc👬"; |] `shouldFailWithError` "file_identifier must be exactly 4 UTF-8 code units\n"
        parseEof schema [r| file_identifier "a👬";   |] `shouldFailWithError` "file_identifier must be exactly 4 UTF-8 code units\n"

        [r| file_identifier "abcd"; |] `parses` Schema [] [ DeclFI "abcd" ]
        [r| file_identifier "👬";   |] `parses` Schema [] [ DeclFI "👬" ]

      it "json objects" $
        [r|
          include "a";

          {
            "a" : 3 ,
            b : "e" ,
            c : [ { d: [ [ ] , [ "a" , null , true , false , - 3 , -239.223e3 ] ] } ]
          }

          attribute b;
        |] `parses`
          Schema
            [ Include "a" ]
            [ DeclA $ AttributeDecl "b" ]

      it "RPC services" $
        [r|
          include "a";

          rpc_service MonsterStorage {
            Store(Monster) : Stat ;
            Retrieve(Stat) : Monster ( streaming : "server" , idempotent ) ;
          }

        |] `parses`
          Schema
            [ Include "a" ] []

shouldFailWithError :: Show a => Either (ParseErrorBundle String Void) a -> String -> Expectation
shouldFailWithError p s =
  case p of
    Left (ParseErrorBundle [x] _) -> parseErrorTextPretty x `shouldBe` s
    Left (ParseErrorBundle xs _)  -> expectationFailure $ "Expected one parsing error, but got more:\n" ++ show xs
    Right a                       -> expectationFailure $ "Expected parsing to fail, but succeeded with:\n" ++ show a

parseEof :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parseEof p = parse (p <* eof) ""

parses :: String -> Schema -> Expectation
parses input expectedSchema =
  case parse schema "" input of
    l@(Left _) -> l `shouldParse` expectedSchema
    Right result -> result `pshouldBe` expectedSchema


