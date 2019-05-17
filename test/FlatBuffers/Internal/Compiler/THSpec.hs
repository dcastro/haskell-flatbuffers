{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}

module FlatBuffers.Internal.Compiler.THSpec where

import           Data.Int
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Word

import qualified FlatBuffers.Internal.Compiler.Parser           as P
import           FlatBuffers.Internal.Compiler.SemanticAnalysis ( SymbolTable(..), validateSchemas )
import           FlatBuffers.Internal.Compiler.SyntaxTree       ( FileTree(..), HasIdent(..), Ident(..), Namespace(..) )
import           FlatBuffers.Internal.Compiler.TH
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree
import           FlatBuffers.Write

import           Language.Haskell.TH.Syntax

import           Test.Hspec

import           TestUtils

import           Text.Megaparsec                                ( ParseErrorBundle, ShowErrorComponent, Stream, errorBundlePretty, parse )
import           Text.RawString.QQ                              ( r )


spec :: Spec
spec =
  describe "TH" $
    describe "Tables" $ do
      describe "naming coventions" $ do
        it "table datatype name is uppercased" $
          [r| table t {}|] `shouldCompileTo`
            [d|
              data T

              t :: WriteTable T
              t = writeTable []
            |]

        it "table constructor name is lowercased" $
          [r| table T {}|] `shouldCompileTo`
            [d|
              data T

              t :: WriteTable T
              t = writeTable []
            |]

        it "table field names are lowercased" $
          [r| table T { X: int; }|] `shouldCompileTo`
            [d|
              data T

              t :: Maybe Int32 -> WriteTable T
              t x = writeTable [ optionalDef 0 (inline int32) x ]
            |]

      describe "numeric fields + boolean" $ do
        it "normal fields" $
          [r|
            table Scalars {
              // scalars
              a: uint8;
              b: uint16;
              c: uint32;
              d: uint64;
              e: int8;
              f: int16;
              g: int32;
              h: int64;
              i: float32;
              j: float64;
              k: bool;
            }
          |] `shouldCompileTo`
            [d|
              data Scalars

              scalars ::
                  Maybe Word8
                -> Maybe Word16
                -> Maybe Word32
                -> Maybe Word64
                -> Maybe Int8
                -> Maybe Int16
                -> Maybe Int32
                -> Maybe Int64
                -> Maybe Float
                -> Maybe Double
                -> Maybe Bool
                -> WriteTable Scalars
              scalars a b c d e f g h i j k =
                writeTable
                  [ optionalDef 0 (inline word8)    a
                  , optionalDef 0 (inline word16)   b
                  , optionalDef 0 (inline word32)   c
                  , optionalDef 0 (inline word64)   d
                  , optionalDef 0 (inline int8)     e
                  , optionalDef 0 (inline int16)    f
                  , optionalDef 0 (inline int32)    g
                  , optionalDef 0 (inline int64)    h
                  , optionalDef 0.0 (inline float)  i
                  , optionalDef 0.0 (inline double) j
                  , optionalDef False (inline bool) k
                  ]
            |]

        it "deprecated fields" $
          [r|
            table Scalars {
              a: uint8 (deprecated);
              b: uint16 (deprecated);
              c: uint32 (deprecated);
              d: uint64 (deprecated);
              e: int8 (deprecated);
              f: int16 (deprecated);
              g: int32 (deprecated);
              h: int64 (deprecated);
              i: float32 (deprecated);
              j: float64 (deprecated);
              k: bool (deprecated);
            }
          |] `shouldCompileTo`
            [d|
              data Scalars

              scalars :: WriteTable Scalars
              scalars =
                writeTable
                  [ deprecated, deprecated, deprecated, deprecated, deprecated, deprecated, deprecated, deprecated, deprecated, deprecated, deprecated
                  ]
            |]

        it "with default values" $
          [r|
            table Scalars {
              // scalars
              a: uint8 = 8;
              b: uint16 = 16;
              c: uint32 = 32;
              d: uint64 = 64;
              e: int8 = -1;
              f: int16 = -2;
              g: int32 = -4;
              h: int64 = -8;
              i: float32 = 3.9;
              j: float64 = -2.3e10;
              k: bool = true;
            }
          |] `shouldCompileTo`
            [d|
              data Scalars

              scalars ::
                  Maybe Word8
                -> Maybe Word16
                -> Maybe Word32
                -> Maybe Word64
                -> Maybe Int8
                -> Maybe Int16
                -> Maybe Int32
                -> Maybe Int64
                -> Maybe Float
                -> Maybe Double
                -> Maybe Bool
                -> WriteTable Scalars
              scalars a b c d e f g h i j k =
                writeTable
                  [ optionalDef 8 (inline word8)          a
                  , optionalDef 16 (inline word16)        b
                  , optionalDef 32 (inline word32)        c
                  , optionalDef 64 (inline word64)        d
                  , optionalDef (-1) (inline int8)        e
                  , optionalDef (-2) (inline int16)       f
                  , optionalDef (-4) (inline int32)       g
                  , optionalDef (-8) (inline int64)       h
                  , optionalDef 3.9 (inline float)        i
                  , optionalDef (-2.3e10) (inline double) j
                  , optionalDef True (inline bool)        k
                  ]
            |]

      describe "string fields" $ do
        it "normal field" $
          [r| table T {s: string;} |] `shouldCompileTo`
            [d|
              data T

              t :: Maybe Text -> WriteTable T
              t s = writeTable [optional text s]
            |]
        it "deprecated" $
          [r| table T {s: string (deprecated);} |] `shouldCompileTo`
            [d|
              data T

              t :: WriteTable T
              t = writeTable [deprecated]
            |]
        it "required" $
          [r| table T {s: string (required);} |] `shouldCompileTo`
            [d|
              data T

              t :: Text -> WriteTable T
              t s = writeTable [text s]
            |]

      describe "table fields" $ do
        it "normal field" $
          [r| table T1 {x: t2;} table t2{}|] `shouldCompileTo`
            [d|
              data T1
              t1 :: Maybe (WriteTable T2) -> WriteTable T1
              t1 x = writeTable [optional unWriteTable x]

              data T2
              t2 :: WriteTable T2
              t2 = writeTable []
            |]
        it "deprecated" $
          [r| table T1 {x: t2 (deprecated) ;} table t2{}|] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteTable T1
              t1 = writeTable [deprecated]

              data T2
              t2 :: WriteTable T2
              t2 = writeTable []
            |]
        it "required" $
          [r| table T1 {x: t2 (required) ;} table t2{}|] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteTable T2 -> WriteTable T1
              t1 x = writeTable [unWriteTable x]

              data T2
              t2 :: WriteTable T2
              t2 = writeTable []
            |]



shouldCompileTo :: HasCallStack => String -> Q [Dec] -> Expectation
shouldCompileTo input expectedQ =
  case parse P.schema "" input of
    Left e       -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = FileTree "" schema mempty
      in  case validateSchemas schemas of
        Left err                  -> expectationFailure $ T.unpack err
        Right (FileTree _ root _) -> do
          ast <- runQ (compileSymbolTable root)
          expected <- runQ expectedQ
          ast `pshouldBe` fmap normalizeDec expected

showBundle :: (ShowErrorComponent e, Stream s) => ParseErrorBundle s e -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x = if null x
      then x
      else "  " ++ x

-- | This function normalize ASTs to make them comparable.
--   * AST obtained from quasiquotes (like what we're doing in these tests) use `newName`, whereas we often use `mkName`.
--     So we have to normalize names here.
--   * Declarations like `x = 5` are interpreted as a value declaration, but they're equivalent to a
--     function declaration with a single clause and a single pattern.
normalizeDec :: Dec -> Dec
normalizeDec dec = valToFun $
  case dec of
    DataD a name b c d e -> DataD a (normalizeName name) b c d e
    SigD n t -> SigD (normalizeName n) (normalizeType t)
    FunD n clauses -> FunD (normalizeName n) (normalizeClause <$> clauses)
    ValD pat body decs -> ValD (normalizePat pat) (normalizeBody body) (normalizeDec <$> decs)
    _ -> dec

-- | values with a simple variable pattern (e.g. `x = 5`) are equivalent to functions with only one clause and no parameters
valToFun :: Dec -> Dec
valToFun dec =
  case dec of
    ValD (VarP name) body decs -> FunD name [Clause [] body decs]
    _ -> dec

normalizeType :: Type -> Type
normalizeType t =
  case t of
    ConT n -> ConT (normalizeName n)
    AppT t1 t2 -> AppT (normalizeType t1) (normalizeType t2)
    _ -> t

normalizeClause :: Clause -> Clause
normalizeClause (Clause pats body decs) = Clause (normalizePat <$> pats) (normalizeBody body) (normalizeDec <$> decs)

normalizePat :: Pat -> Pat
normalizePat p =
  case p of
    VarP n -> VarP (normalizeName n)
    _ -> p


normalizeBody :: Body -> Body
normalizeBody b =
  case b of
    NormalB e -> NormalB (normalizeExp e)
    _ -> b

normalizeExp :: Exp -> Exp
normalizeExp e =
  case e of
    VarE n -> VarE (normalizeName n)
    AppE e1 e2 -> AppE (normalizeExp e1) (normalizeExp e2)
    ListE es -> ListE (normalizeExp <$> es)
    _ -> e

normalizeName (Name (OccName occ) (NameU _)) = mkName occ
normalizeName name = name

