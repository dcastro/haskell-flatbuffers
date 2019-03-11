{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FlatBuffers.Internal.Compiler.SemanticAnalysisSpec where

import           Data.Foldable                                  (fold)
import           Data.Int
import           Data.List.NonEmpty                             (fromList)
import           Data.Maybe                                     (mapMaybe)
import           Data.Text                                      (Text)
import qualified Data.Tree                                      as Tree
import qualified FlatBuffers.Internal.Compiler.Parser           as P
import           FlatBuffers.Internal.Compiler.SemanticAnalysis
import qualified FlatBuffers.Internal.Compiler.SyntaxTree       as ST
import           Test.Hspec
import           Text.Megaparsec
import           Text.RawString.QQ                              (r)

spec :: Spec
spec =
  describe "SemanticAnalysis" $
    describe "enums" $ do
      it "simple" $
        [r|
          namespace Ns;
          enum Color : uint32 { Red, Green, Blue }
        |] `shouldValidate`
          enum (EnumDecl "Ns" "Color" EWord32 $ fromList
            [ EnumVal "Red" 0
            , EnumVal "Green" 1
            , EnumVal "Blue" 2
            ])
      
      it "multiple enums in different namespaces" $
        [r|
          namespace A;
          enum Color1 : uint32 { Red }

          namespace B;
          namespace ;
          enum Color2 : uint32 { Green }

          namespace A.B.C;
          enum Color3 : uint32 { Blue }

        |] `shouldValidate` fold
          [ enum (EnumDecl "A" "Color1" EWord32 $ fromList [EnumVal "Red" 0] )
          , enum (EnumDecl "" "Color2" EWord32 $ fromList [EnumVal "Green" 0] )
          , enum (EnumDecl "A.B.C" "Color3" EWord32 $ fromList [EnumVal "Blue" 0] )
          ]

      it "with explicit values" $
        [r| enum Color : int32 { Red = -2, Green, Blue = 2 } |] `shouldValidate`
          enum (EnumDecl "" "Color" EInt32 $ fromList
            [ EnumVal "Red" (-2)
            , EnumVal "Green" (-1)
            , EnumVal "Blue" 2
            ])

      it "with explicit values (min/maxBound)" $
        [r| enum Color : int8 { Red = -128, Green, Blue = 127 } |] `shouldValidate`
          enum (EnumDecl "" "Color" EInt8 $ fromList
          [ EnumVal "Red" (toInteger (minBound :: Int8))
          , EnumVal "Green" (-127) 
          , EnumVal "Blue" (toInteger (maxBound :: Int8))
          ])

      it "with out-of-bounds values" $ do
        [r|
          namespace A.B;
          enum Color : int8 { Red = -129, Green, Blue }
        |] `shouldFail`
          "[A.B.Color]: enum value does not fit [-128; 127]"
        [r|
          enum Color : int8 { Red, Green, Blue = 128 }
        |] `shouldFail`
          "[Color]: enum value does not fit [-128; 127]"

      it "with values out of order" $ do
        [r| enum Color : int8 { Red = 3, Green = 2, Blue } |] `shouldFail`
          "[Color]: enum values must be specified in ascending order"
        [r| enum Color : int8 { Red = 3, Green = 3, Blue } |] `shouldFail`
          "[Color]: enum values must be specified in ascending order"

      it "with bit_flags" $
        [r| enum Color : int8 (bit_flags) { Red, Green, Blue } |] `shouldFail`
          "[Color]: `bit_flags` are not supported yet"

      it "with duplicate values" $
        [r| enum Color : int8 { Red, Green, Red, Gray, Green, Green, Black } |] `shouldFail`
          "[Color]: [Green, Red] declared more than once"

      it "with invalid underlying type" $ do
        [r| enum Color : double { Red, Green, Blue } |] `shouldFail`
          "[Color]: underlying enum type must be integral"
        [r| enum Color : TypeRef { Red, Green, Blue } |] `shouldFail`
          "[Color]: underlying enum type must be integral"
        [r| enum Color : [int] { Red, Green, Blue } |] `shouldFail`
          "[Color]: underlying enum type must be integral"

enum :: EnumDecl -> ValidatedDecls
enum e = ValidatedDecls [e] []

struct :: StructDecl -> ValidatedDecls
struct s = ValidatedDecls [] [s]

shouldValidate :: String -> ValidatedDecls -> Expectation
shouldValidate input expectation =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = Tree.Node schema []
          declsWithNamespaces = pairDeclsWithNamespaces schemas
      in  validateDecls declsWithNamespaces `shouldBe` Right expectation

shouldFail :: String -> Text -> Expectation
shouldFail input expectedErrorMsg =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = Tree.Node schema []
          declsWithNamespaces = pairDeclsWithNamespaces schemas
      in  validateDecls declsWithNamespaces `shouldBe` Left expectedErrorMsg

showBundle :: ( ShowErrorComponent e, Stream s) => ParseErrorBundle s e -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x = if null x
      then x
      else "  " ++ x
