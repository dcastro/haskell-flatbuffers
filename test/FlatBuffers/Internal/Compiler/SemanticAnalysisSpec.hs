{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE QuasiQuotes       #-}

module FlatBuffers.Internal.Compiler.SemanticAnalysisSpec where

import           Data.Foldable                                  (fold)
import           Data.Int
import           Data.Maybe                                     (mapMaybe)
import           Data.Text                                      (Text)
import qualified Data.Tree                                      as Tree
import qualified FlatBuffers.Internal.Compiler.Parser           as P
import           FlatBuffers.Internal.Compiler.SemanticAnalysis
import qualified FlatBuffers.Internal.Compiler.SyntaxTree       as ST
import           FlatBuffers.Internal.Compiler.SyntaxTree       (Namespace)
import           Test.Hspec
import           Text.Megaparsec
import           Text.RawString.QQ                              (r)

spec :: Spec
spec =
  describe "SemanticAnalysis" $ do
    describe "enums" $ do
      it "simple" $
        [r|
          namespace Ns;
          enum Color : uint32 { Red, Green, Blue }
        |] `shouldValidate`
          enum ("Ns", EnumDecl "Color" EWord32
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

        |] `shouldValidate` foldDecls
          [ enum ("A",      EnumDecl "Color1" EWord32 [EnumVal "Red" 0] )
          , enum ("",       EnumDecl "Color2" EWord32 [EnumVal "Green" 0] )
          , enum ("A.B.C",  EnumDecl "Color3" EWord32 [EnumVal "Blue" 0] )
          ]

      it "with explicit values" $
        [r| enum Color : int32 { Red = -2, Green, Blue = 2 } |] `shouldValidate`
          enum ("", EnumDecl "Color" EInt32
            [ EnumVal "Red" (-2)
            , EnumVal "Green" (-1)
            , EnumVal "Blue" 2
            ])

      it "with explicit values (min/maxBound)" $
        [r| enum Color : int8 { Red = -128, Green, Blue = 127 } |] `shouldValidate`
          enum ("", EnumDecl "Color" EInt8
          [ EnumVal "Red" (toInteger (minBound :: Int8))
          , EnumVal "Green" (-127) 
          , EnumVal "Blue" (toInteger (maxBound :: Int8))
          ])

      it "with out-of-bounds values" $ do
        [r|
          namespace A.B;
          enum Color : int8 { Red = -129, Green, Blue }
        |] `shouldFail`
          "[A.B.Color.Red]: enum value does not fit [-128; 127]"
        [r|
          enum Color : int8 { Red, Green, Blue = 128 }
        |] `shouldFail`
          "[Color.Blue]: enum value does not fit [-128; 127]"

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

    describe "structs" $ do
      it "simple" $
        [r|
          namespace Ns;
          struct S {
            x: int;
          }
        |] `shouldValidate`
          struct ("Ns", StructDecl "S" 4
            [ StructField "x" SInt32
            ])

      it "multiple fields" $
        [r|
          struct S {
            x: ubyte;
            y: double;
            z: bool;
          }
        |] `shouldValidate`
          struct ("", StructDecl "S" 8
            [ StructField "x" SWord8
            , StructField "y" SDouble
            , StructField "z" SBool
            ])

      it "when unqualified TypeRef is ambiguous, types in namespaces closer to the struct are preferred" $ do
        let enumVal = EnumVal "x" 0
            mkEnum namespace ident = enum (namespace, EnumDecl ident EInt16 [enumVal])
        [r|
          namespace ;       enum E1 : short{x}   enum E2 : short{x}   enum E3 : short{x}
          namespace A;      enum E1 : short{x}   enum E2 : short{x}
          namespace A.B;    enum E1 : short{x}
          namespace A.B.C;  enum E1 : short{x}   enum E2 : short{x}   enum E3 : short{x}

          namespace A.B;
          struct S {
            x: E1; // should be A.B.E1
            y: E2; // should be A.E2
            z: E3; // should be E3
          }
        |] `shouldValidate` foldDecls
          [ mkEnum ""       "E1", mkEnum ""       "E2", mkEnum ""       "E3"
          , mkEnum "A"      "E1", mkEnum "A"      "E2"
          , mkEnum "A.B"    "E1"
          , mkEnum "A.B.C"  "E1", mkEnum "A.B.C"  "E2", mkEnum "A.B.C"  "E3"
          , struct ("A.B", StructDecl "S" 2
              [ StructField "x" (SEnum "A.B"  "E1" EInt16)
              , StructField "y" (SEnum "A"    "E2" EInt16)
              , StructField "z" (SEnum ""     "E3" EInt16)
              ])
          ]

      it "when qualified TypeRef is ambiguous, types in namespaces closer to the struct are preferred" $ do
        let enumVal = EnumVal "x" 0
            mkEnum namespace ident = enum (namespace, EnumDecl ident EInt16 [enumVal])
        [r|
          namespace ;         enum E1 : short{x}   enum E2 : short{x}   enum E3 : short{x}
          namespace A;        enum E1 : short{x}   enum E2 : short{x}   enum E3 : short{x}
          namespace A.B;      enum E1 : short{x}   enum E2 : short{x}   enum E3 : short{x}
          namespace A.A;      enum E1 : short{x}   enum E2 : short{x}
          namespace A.B.A;    enum E1 : short{x}
          namespace A.B.C.A;  enum E1 : short{x}   enum E2 : short{x}   enum E3 : short{x}

          namespace A.B;
          struct S {
            x: A.E1; // should be A.B.A.E1
            y: A.E2; // should be A.A.E2
            z: A.E3; // should be A.E3
          }
        |] `shouldValidate` foldDecls
          [ mkEnum ""         "E1", mkEnum ""         "E2", mkEnum ""         "E3"
          , mkEnum "A"        "E1", mkEnum "A"        "E2", mkEnum "A"        "E3"
          , mkEnum "A.B"      "E1", mkEnum "A.B"      "E2", mkEnum "A.B"      "E3"
          , mkEnum "A.A"      "E1", mkEnum "A.A"      "E2"
          , mkEnum "A.B.A"    "E1"
          , mkEnum "A.B.C.A"  "E1", mkEnum "A.B.C.A"  "E2", mkEnum "A.B.C.A"  "E3"
          , struct ("A.B", StructDecl "S" 2
              [ StructField "x" (SEnum "A.B.A" "E1" EInt16)
              , StructField "y" (SEnum "A.A"   "E2" EInt16)
              , StructField "z" (SEnum "A"     "E3" EInt16)
              ])
          ]

      it "when TypeRef is ambiguous, types in namespaces closer to the struct are preferred, even if they're not valid" $
        [r|
          namespace A;    struct X {x: int;}
          namespace A.B;  table X {}

          struct S {
            x: X;
          }
        |] `shouldFail`
          "[A.B.S.x]: struct fields may only be integers, floating point, bool, enums, or other structs"

      it "with field referencing an enum" $
        [r|
          namespace A;
          enum Color : ushort { Blue }

          struct S {
            x: Color;
          }
        |] `shouldValidate` foldDecls
          [ enum ("A", EnumDecl "Color" EWord16 [EnumVal "Blue" 0])
          , struct ("A", StructDecl "S" 2
              [ StructField "x" (SEnum "A" "Color" EWord16)
              ])
          ]

      it "with nested structs (backwards/forwards references)" $ do
        let backwards = ("A.B", StructDecl "Backwards" 4 [ StructField "x" SFloat ])
        let forwards = ("A.B", StructDecl "Forwards" 4 [ StructField "y" (SStruct backwards) ])
        [r|
          namespace A.B;
          struct Backwards {
            x: float;
          }

          struct S {
            x1: B.Backwards;
            x2: A.B.Forwards;
          }

          struct Forwards {
            y: Backwards;
          }
        |] `shouldValidate` foldDecls
          [ struct ("A.B", StructDecl "S" 4
              [ StructField "x1" (SStruct backwards)
              , StructField "x2" (SStruct forwards)
              ])
          , struct forwards
          , struct backwards
          ]

      it "with reference to a table" $
        [r|
          namespace A;
          table T {}
          
          struct S {
            x: A.T;
          }
        |] `shouldFail`
          "[A.S.x]: struct fields may only be integers, floating point, bool, enums, or other structs"

      it "with reference to a union" $
        [r|
          namespace A.B;
          union U { X }
          
          struct S {
            x: U;
          }
        |] `shouldFail`
         "[A.B.S.x]: struct fields may only be integers, floating point, bool, enums, or other structs"

      it "with invalid reference" $
        [r|
          namespace X.Y.Z;
          struct S {
            x: A.T;
          }
        |] `shouldFail`
          "[X.Y.Z.S.x]: type 'A.T' does not exist (checked in these namespaces: ['X.Y.Z', 'X.Y', 'X', ''])"

      it "with reference to a vector" $
        [r| struct S { x: [byte]; } |] `shouldFail`
         "[S.x]: struct fields may only be integers, floating point, bool, enums, or other structs"

      it "with reference to a string" $
        [r| struct S { x: string; } |] `shouldFail`
          "[S.x]: struct fields may only be integers, floating point, bool, enums, or other structs"

      it "with duplicate fields" $
        [r| struct S { x: byte; x: int; } |] `shouldFail`
          "[S]: [x] declared more than once"

      it "with `force_align` attribute" $ do
        -- just 1 field
        [r| struct S (force_align: 4) { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 4 [StructField "x" SInt32])
        [r| struct S (force_align: 8) { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 8 [StructField "x" SInt32])
        [r| struct S (force_align: 16) { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 16 [StructField "x" SInt32])
        -- multiple fields
        [r| struct S (force_align: 2) { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 2 [StructField "x" SInt8, StructField "y" SWord16])
        [r| struct S (force_align: 4) { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 4 [StructField "x" SInt8, StructField "y" SWord16])
        [r| struct S (force_align: 8) { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 8 [StructField "x" SInt8, StructField "y" SWord16])
        [r| struct S (force_align: 16) { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 16 [StructField "x" SInt8, StructField "y" SWord16])
        -- nested structs
        let s1 = ("", StructDecl "S1" 2 [StructField "x" SInt8])
        let s2 = ("", StructDecl "S2" 4 [StructField "x" SInt32])
        [r|
          struct S (force_align: 4) { x: S1; y: S2; z: bool; }
          struct S1 (force_align: 2) { x: byte; }
          struct S2 { x: int; }
        |] `shouldValidate` foldDecls
          [ struct ("", StructDecl "S" 4 [StructField "x" (SStruct s1), StructField "y" (SStruct s2), StructField "z" SBool])
          , struct s2, struct s1
          ]

      it "with `force_align` attribute less than the struct's natural alignment" $
        [r| struct S (force_align: 2) { x: byte; y: int; } |] `shouldFail`
          "[S]: force_align must be a power of two integer ranging from the struct's natural alignment (in this case, 4) to 16"

      it "with `force_align` attribute greater than 16" $
        [r| struct S (force_align: 32) { x: int; } |] `shouldFail`
          "[S]: force_align must be a power of two integer ranging from the struct's natural alignment (in this case, 4) to 16"

      it "with `force_align` not a power of 2" $
        [r| struct S (force_align: 9) { x: int; } |] `shouldFail`
          "[S]: force_align must be a power of two integer ranging from the struct's natural alignment (in this case, 4) to 16"

      it "with `force_align` given as a string" $
        [r| struct S (force_align: "2") { x: byte; } |] `shouldFail`
          "[S]: expected attribute 'force_align' to have an integer value, e.g. 'force_align: 123'"

      it "with deprecated field" $ 
        [r| struct S { x: byte (deprecated); } |] `shouldFail`
          "[S.x]: can't deprecate fields in a struct"

      it "with cyclic dependency" $
        [r|
          struct S {x: S1;}
          struct S1 {x: S4;   y: S2;}
          struct S2 {x: byte; y: S3;}
          struct S3 {x: S4;   y: S1;}
          struct S4 {x: byte;}
        |] `shouldFail`
          "[S1]: cyclic dependency detected [S1 -> S2 -> S3 -> S1] - structs cannot contain themselves, directly or indirectly"


foldDecls :: [Stage3] -> Stage3
foldDecls = fold

enum :: (Namespace, EnumDecl) -> Stage3
enum e = SymbolTable [e] [] [] []

struct :: (Namespace, StructDecl) -> Stage3
struct s = SymbolTable [] [s] [] []

shouldValidate :: String -> Stage3 -> Expectation
shouldValidate input expectation =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = Tree.Node schema []
          symbolTable = createSymbolTable schemas
      in  validateDecls symbolTable `shouldBe` Right expectation

shouldFail :: String -> Text -> Expectation
shouldFail input expectedErrorMsg =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = Tree.Node schema []
          symbolTable = createSymbolTable schemas
      in  validateDecls symbolTable `shouldBe` Left expectedErrorMsg

showBundle :: ( ShowErrorComponent e, Stream s) => ParseErrorBundle s e -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x = if null x
      then x
      else "  " ++ x
