{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.Compiler.ParserSpec where

import           Data.List.NonEmpty
import           Data.Void             (Void)
import           FlatBuffers.Compiler.Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

spec :: Spec
spec =
  describe "include" $ do
    it "parses correctly" $
      parseEof include "include \"abc\";" `shouldParse` "abc"
    it "parses strings with semicolons" $
      parseEof include "include \"abc;\";" `shouldParse` "abc;"
    it "parses escaped strings" $ do
      parseEof include "include \"abc \\\" \" ;" `shouldParse` "abc \" "
      parseEof include "include \"abc \\\" escaped \\\" rest\" ;" `shouldParse` "abc \" escaped \" rest"
    describe "fails to parse" $ do
      it "unmatched quotes" $ 
        parseEof include "include \"abc;" `shouldFailWithError` "unexpected end of input\nexpecting '\"' or literal character\n"
      it "more than one string constant" $
        parseEof include "include \"abc\" \"def\";" `shouldFailWithError` "unexpected '\"'\nexpecting ';'\n"
      it "if there's no semicolon" $
        parseEof include "include \"abc\"" `shouldFailWithError` "unexpected end of input\nexpecting ';'\n"

shouldFailWithError :: Show a => Either (ParseErrorBundle String Void) a -> String -> Expectation
shouldFailWithError p s =
  case p of
    Left (ParseErrorBundle (x :| []) _) -> parseErrorTextPretty x `shouldBe` s
    Left (ParseErrorBundle xs _)        -> fail $ "Expected one parsing error, but got more:\n" ++ show xs
    Right a                             -> fail $ "Expected parsing to fail, but succeeded with:\n" ++ show a

parseEof :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parseEof p = parse (p <* eof) ""
