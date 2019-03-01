{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FlatBuffers.Internal.Compiler.ParserSpec where

import           Data.List.NonEmpty
import           Data.Void                                (Void)
import           FlatBuffers.Internal.Compiler.Parser
import           FlatBuffers.Internal.Compiler.SyntaxTree
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.RawString.QQ                        (r)

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

      it "namespaces" $
        [r|
          include "somefile";
          namespace Ns;
          namespace My . Api . Domain;
          namespace My.Api.Domain2;
        |] `parses`
          Schema
            ["somefile"]
            [ DeclN $ NamespaceDecl $ fromList ["Ns"]
            , DeclN $ NamespaceDecl $ fromList ["My", "Api", "Domain"]
            , DeclN $ NamespaceDecl $ fromList ["My", "Api", "Domain2"]
            ]

      it "table declarations" $
        [r|
          table T {}
          
          table ATable {
            abc : bool;
            b1 : bool = true;
            b2 : bool = false;
            d : Ref = 123;
            e : [uint] = 99.2e9;
            f : [abc_];
            g : My . Api . Ref = 123;
            h : [ MyApi.abc_ ] ;
            i: Color = Blue ;
          }
        |] `parses`
          Schema
            []
            [ DeclT $ TableDecl "T" (Metadata []) []
            , DeclT $ TableDecl "ATable" (Metadata [])
              [ TableField "abc" TBool Nothing (Metadata [])
              , TableField "b1" TBool (Just (DefaultB True)) (Metadata [])
              , TableField "b2" TBool (Just (DefaultB False)) (Metadata [])
              , TableField "d" (TRef (TypeRef "" "Ref")) (Just (DefaultN "123")) (Metadata [])
              , TableField "e" (TVector TWord32) (Just (DefaultN "99.2e9")) (Metadata [])
              , TableField "f" (TVector (TRef (TypeRef "" "abc_"))) Nothing (Metadata [])
              , TableField "g" (TRef (TypeRef "My.Api" "Ref")) (Just (DefaultN "123")) (Metadata [])
              , TableField "h" (TVector (TRef (TypeRef "MyApi" "abc_"))) Nothing (Metadata [])
              , TableField "i" (TRef (TypeRef "" "Color")) (Just (DefaultI "Blue")) (Metadata [])
              ]
            ]

      it "struct declarations" $
        [r|
          struct AStruct {
            abc : bool;
            d : Ref ;
            e : [uint] ;
            f : [abc_];
            g : My . Api . Ref ;
            h : [ MyApi.abc_ ] ;
          }
        |] `parses`
          Schema
            []
            [ DeclS $ StructDecl "AStruct" (Metadata []) $ fromList
              [ StructField "abc" TBool (Metadata [])
              , StructField "d" (TRef (TypeRef "" "Ref")) (Metadata [])
              , StructField "e" (TVector TWord32) (Metadata [])
              , StructField "f" (TVector (TRef (TypeRef "" "abc_"))) (Metadata [])
              , StructField "g" (TRef (TypeRef "My.Api" "Ref")) (Metadata [])
              , StructField "h" (TVector (TRef (TypeRef "MyApi" "abc_"))) (Metadata [])
              ]
            ]

      it "table declarations with metadata" $
        [r|
          table ATable ( a , "b" : 99992873786287637862.298736756627897654e999999 , c : 3 , d : "attr" ) {
            abc : bool = 99 ( def ) ;
          }
        |] `parses`
          Schema
            []
            [ DeclT $ TableDecl "ATable"
              (Metadata
                [ ("a", Nothing)
                , ("b", Just (LiteralN "99992873786287637862.298736756627897654e999999"))
                , ("c", Just (LiteralN "3"))
                , ("d", Just (LiteralS "attr"))
                ]
              )
              (pure (TableField "abc" TBool (Just (DefaultN "99")) (Metadata [("def", Nothing)])))
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
            [DeclE $ EnumDecl "Color" TInt16 (Metadata [("attr", Nothing)]) $ fromList
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
                (fromList
                  [ UnionVal Nothing (TypeRef "" "Sword")
                  , UnionVal (Just "mace") (TypeRef "" "Stick")
                  , UnionVal (Just "mace2") (TypeRef "My.Api" "Stick")
                  , UnionVal Nothing (TypeRef "" "Axe")
                  ]
                )
            ]

      it "root types, file extensions / identifiers, attribute declarations" $
        [r|
          attribute a;
          attribute "b";
          root_type c;
          root_type My.Api.C ;
          file_extension "d";
          file_identifier "e";
        |] `parses`
          Schema
            []
            [ DeclA $ AttributeDecl "a"
            , DeclA $ AttributeDecl "b"
            , DeclR $ RootDecl (TypeRef "" "c")
            , DeclR $ RootDecl (TypeRef "My.Api" "C")
            , DeclFI $ FileIdentifierDecl "e"
            ]

      it "json objects" $
        [r|
          include "a";

          {
            "a" : 3 ,
            b : "e" ,
            c : [ { d: [ [ ] , [ "a" , null , true , false , -3 , -239.223e3 ] ] } ]
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


