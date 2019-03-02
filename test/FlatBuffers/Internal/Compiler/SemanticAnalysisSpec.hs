{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FlatBuffers.Internal.Compiler.SemanticAnalysisSpec where

import           Data.Int
import           Data.List.NonEmpty                             (fromList)
import           Data.Maybe                                     (mapMaybe)
import           Data.Text                                      (Text)
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
        [r| enum Color : uint32 { Red, Green, Blue } |] `validatesEnum`
          Right (EnumDecl "Color" EWord32 $ fromList
            [ EnumVal "Red" 0
            , EnumVal "Green" 1
            , EnumVal "Blue" 2
            ])
      it "with explicit values" $
        [r| enum Color : int32 { Red = -2, Green, Blue = 2 } |] `validatesEnum`
          Right (EnumDecl "Color" EInt32 $ fromList
            [ EnumVal "Red" (-2)
            , EnumVal "Green" (-1)
            , EnumVal "Blue" 2
            ])
      it "with explicit values (min/maxBound)" $
        [r| enum Color : int8 { Red = -128, Green, Blue = 127 } |] `validatesEnum`
          Right (EnumDecl "Color" EInt8 $ fromList
          [ EnumVal "Red" (toInteger (minBound :: Int8))
          , EnumVal "Green" (-127) 
          , EnumVal "Blue" (toInteger (maxBound :: Int8))
          ])
      it "with out-of-bounds values" $ do
        [r| enum Color : int8 { Red = -129, Green, Blue } |] `validatesEnum`
          Left "[Ns.Color]: enum value does not fit [-128; 127]"
        [r| enum Color : int8 { Red, Green, Blue = 128 } |] `validatesEnum`
          Left "[Ns.Color]: enum value does not fit [-128; 127]"

      it "with values out of order" $ do
        [r| enum Color : int8 { Red = 3, Green = 2, Blue } |] `validatesEnum`
          Left "[Ns.Color]: enum values must be specified in ascending order"
        [r| enum Color : int8 { Red = 3, Green = 3, Blue } |] `validatesEnum`
          Left "[Ns.Color]: enum values must be specified in ascending order"

      it "with bit_flags" $
        [r| enum Color : int8 (bit_flags) { Red, Green, Blue } |] `validatesEnum`
          Left "[Ns.Color]: `bit_flags` are not supported yet"

      it "with duplicate values" $
        [r| enum Color : int8 { Red, Green, Red, Gray, Green, Green, Black } |] `validatesEnum`
          Left "[Ns.Color]: fields [Green, Red] declared more than once"

      it "with invalid underlying type" $ do
        [r| enum Color : double { Red, Green, Blue } |] `validatesEnum`
          Left "[Ns.Color]: underlying enum type must be integral"
        [r| enum Color : TypeRef { Red, Green, Blue } |] `validatesEnum`
          Left "[Ns.Color]: underlying enum type must be integral"
        [r| enum Color : [int] { Red, Green, Blue } |] `validatesEnum`
          Left "[Ns.Color]: underlying enum type must be integral"


validatesEnum :: String -> Either Text EnumDecl -> Expectation
validatesEnum input expectation =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right result -> validateEnum "Ns" (head $ mapMaybe filterEnum $ ST.decls result) `shouldBe` expectation
  where
    filterEnum (ST.DeclE e) = Just e
    filterEnum _            = Nothing
    
showBundle :: ( ShowErrorComponent e, Stream s) => ParseErrorBundle s e -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x = if null x
      then x
      else "  " ++ x