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
import           FlatBuffers.Internal.Compiler.SyntaxTree       (Namespace, TypeRef(..))
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
            [ StructField "x" 0 SInt32
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
            [ StructField "x" 7 SWord8
            , StructField "y" 0 SDouble
            , StructField "z" 7 SBool
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
              [ StructField "x" 0 (SEnum "A.B"  "E1" EInt16)
              , StructField "y" 0 (SEnum "A"    "E2" EInt16)
              , StructField "z" 0 (SEnum ""     "E3" EInt16)
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
              [ StructField "x" 0 (SEnum "A.B.A" "E1" EInt16)
              , StructField "y" 0 (SEnum "A.A"   "E2" EInt16)
              , StructField "z" 0 (SEnum "A"     "E3" EInt16)
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
              [ StructField "x" 0 (SEnum "A" "Color" EWord16)
              ])
          ]

      it "with nested structs (backwards/forwards references)" $ do
        let backwards = ("A.B", StructDecl "Backwards" 4 [ StructField "x" 0 SFloat ])
        let forwards = ("A.B", StructDecl "Forwards" 4 [ StructField "y" 0 (SStruct backwards) ])
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
              [ StructField "x1" 0 (SStruct backwards)
              , StructField "x2" 0 (SStruct forwards)
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
        [r| struct S (force_align: 4)  { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 4  [StructField "x" 0 SInt32])
        [r| struct S (force_align: 8)  { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 8  [StructField "x" 4 SInt32])
        [r| struct S (force_align: 16) { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 16 [StructField "x" 12 SInt32])
        -- multiple fields
        [r| struct S (force_align: 2)  { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 2  [StructField "x" 1 SInt8, StructField "y" 0 SWord16])
        [r| struct S (force_align: 4)  { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 4  [StructField "x" 1 SInt8, StructField "y" 0 SWord16])
        [r| struct S (force_align: 8)  { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 8  [StructField "x" 1 SInt8, StructField "y" 4 SWord16])
        [r| struct S (force_align: 16) { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 16 [StructField "x" 1 SInt8, StructField "y" 12 SWord16])
        -- nested structs
        let s1 = ("", StructDecl "S1" 2 [StructField "x" 1 SInt8])
        let s2 = ("", StructDecl "S2" 4 [StructField "x" 0 SInt32])
        [r|
          struct S (force_align: 4) { x: S1; y: S2; z: bool; }
          struct S1 (force_align: 2) { x: byte; }
          struct S2 { x: int; }
        |] `shouldValidate` foldDecls
          [ struct ("", StructDecl "S" 4 [StructField "x" 2 (SStruct s1), StructField "y" 0 (SStruct s2), StructField "z" 3 SBool])
          , struct s2
          , struct s1
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
        [r| struct S (force_align: "hello") { x: byte; } |] `shouldFail`
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

    describe "tables" $ do
      it "empty" $
        [r| table T{} |] `shouldValidate` table ("", TableDecl "T" [])
      describe "with numeric/bool fields" $ do
        it "simple" $
          [r|
            namespace A.B;
            table T {
              a: byte;
              b: short;
              c: int;
              d: long;
              e: ubyte;
              f: ushort;
              g: uint;
              h: ulong;
              i: float;
              j: double;
              k: bool;
            }
          |] `shouldValidate`
            table ("A.B", TableDecl "T"
              [ TableField "a" (TInt8 0) False
              , TableField "b" (TInt16 0) False
              , TableField "c" (TInt32 0) False
              , TableField "d" (TInt64 0) False
              , TableField "e" (TWord8 0) False
              , TableField "f" (TWord16 0) False
              , TableField "g" (TWord32 0) False
              , TableField "h" (TWord64 0) False
              , TableField "i" (TFloat 0) False
              , TableField "j" (TDouble 0) False
              , TableField "k" (TBool (DefaultVal False)) False
              ]
            )

        it "with `required` attribute" $ do
          let errorMsg = "[T.x]: only non-scalar fields (strings, vectors, unions, structs, tables) may be 'required'"
          [r| table T {x: byte    (required); } |] `shouldFail` errorMsg
          [r| table T {x: short   (required); } |] `shouldFail` errorMsg
          [r| table T {x: int     (required); } |] `shouldFail` errorMsg
          [r| table T {x: long    (required); } |] `shouldFail` errorMsg
          [r| table T {x: ubyte   (required); } |] `shouldFail` errorMsg
          [r| table T {x: ushort  (required); } |] `shouldFail` errorMsg
          [r| table T {x: uint    (required); } |] `shouldFail` errorMsg
          [r| table T {x: ulong   (required); } |] `shouldFail` errorMsg
          [r| table T {x: float   (required); } |] `shouldFail` errorMsg
          [r| table T {x: double  (required); } |] `shouldFail` errorMsg
          [r| table T {x: bool    (required); } |] `shouldFail` errorMsg

        it "with `deprecated` attribute" $
          [r|
            namespace A.B;
            table T {
              a: byte (deprecated);
              b: short (deprecated);
              c: int (deprecated);
              d: long (deprecated);
              e: ubyte (deprecated);
              f: ushort (deprecated);
              g: uint (deprecated);
              h: ulong (deprecated);
              i: float (deprecated);
              j: double (deprecated);
              k: bool (deprecated);
            }
          |] `shouldValidate`
            table ("A.B", TableDecl "T"
              [ TableField "a" (TInt8 0) True
              , TableField "b" (TInt16 0) True
              , TableField "c" (TInt32 0) True
              , TableField "d" (TInt64 0) True
              , TableField "e" (TWord8 0) True
              , TableField "f" (TWord16 0) True
              , TableField "g" (TWord32 0) True
              , TableField "h" (TWord64 0) True
              , TableField "i" (TFloat 0) True
              , TableField "j" (TDouble 0) True
              , TableField "k" (TBool (DefaultVal False)) True
              ]
            )

      describe "with integer fields" $ do
        it "with integer default values" $
          [r|
            table T {
              a: byte = 127;
              b: short = -32768;
              c: int = 1;
              d: long = 1.00;
              e: ubyte = 2e1;
              f: ushort = 200e-1;
            }
          |] `shouldValidate`
            table ("", TableDecl "T"
              [ TableField "a" (TInt8 127) False
              , TableField "b" (TInt16 (-32768)) False
              , TableField "c" (TInt32 1) False
              , TableField "d" (TInt64 1) False
              , TableField "e" (TWord8 20) False
              , TableField "f" (TWord16 20) False
              ]
            )

        it "with out-of-bounds default values" $ do
          [r| table T { a: byte = -129; } |] `shouldFail` "[T.a]: default value does not fit [-128; 127]"
          [r| table T { a: byte = 128;  } |] `shouldFail` "[T.a]: default value does not fit [-128; 127]"

        let errorMsg = "[T.a]: default value must be integral"
        it "with decimal default values" $ do
          [r| table T { a: byte = 1.1;    } |] `shouldFail` errorMsg
          [r| table T { a: byte = 2e-1;   } |] `shouldFail` errorMsg
          [r| table T { a: byte = 2.22e1; } |] `shouldFail` errorMsg

        it "with boolean default values" $
          [r| table T { a: byte = true; } |] `shouldFail` errorMsg

        it "with identifier default values" $
          [r| table T { a: byte = Red; } |] `shouldFail` errorMsg
          
      describe "with floating point fields" $ do
        it "with integer default values" $
          [r|
            table T {
              a: float = 127;
              b: float = -32768;
              c: float = 1;
              d: double = 1.00;
              e: double = 2e1;
              f: double = 200e-1;
            }
          |] `shouldValidate`
            table ("", TableDecl "T"
              [ TableField "a" (TFloat 127) False
              , TableField "b" (TFloat (-32768)) False
              , TableField "c" (TFloat 1) False
              , TableField "d" (TDouble 1) False
              , TableField "e" (TDouble 20) False
              , TableField "f" (TDouble 20) False
              ]
            )

        it "with decimal default values" $
          [r|
            table T {
              a: double = 1.1;
              b: double = 2e-1;
              c: double = 2.22e1;
            }
          |] `shouldValidate`
            table ("", TableDecl "T"
              [ TableField "a" (TDouble 1.1) False
              , TableField "b" (TDouble 0.2) False
              , TableField "c" (TDouble 22.2) False
              ]
            )

        it "with boolean default values" $
          [r| table T { a: double = true; } |] `shouldFail` "[T.a]: default value must be a number"

        it "with identifier default values" $
          [r| table T { a: double = Red; } |] `shouldFail` "[T.a]: default value must be a number"

      describe "with boolean fields" $ do
        it "with integer default values" $
          [r| table T { a: bool = 1; } |] `shouldFail` "[T.a]: default value must be a boolean"
          
        it "with decimal default values" $
          [r| table T { a: bool = 1.1; } |] `shouldFail` "[T.a]: default value must be a boolean"

        it "with boolean default values" $
          [r|
            table T {
              a: bool = true;
              b: bool = false;
            }
          |] `shouldValidate`
            table ("", TableDecl "T"
              [ TableField "a" (TBool (DefaultVal True)) False
              , TableField "b" (TBool (DefaultVal False)) False
              ]
            )

        it "with identifier default values" $
          [r| table T { a: bool = Red; } |] `shouldFail` "[T.a]: default value must be a boolean"

      describe "with string fields" $ do
        it "simple" $
          [r| table T { x: string; } |] `shouldValidate`
            table ("", TableDecl "T" [ TableField "x" (TString Opt) False ])
        it "with `required` attribute" $
          [r| table T { x: string (required); } |] `shouldValidate`
            table ("", TableDecl "T" [ TableField "x" (TString Req) False ])
        it "with `deprecated` attribute" $
          [r| table T { x: string (deprecated); } |] `shouldValidate`
            table ("", TableDecl "T" [ TableField "x" (TString Opt) True ])
        it "with default value" $ do
          let errorMsg = "[T.x]: default values currently only supported for scalar fields (integers, floating point, bool, enums)"
          [r| table T { x: string = a;   } |] `shouldFail` errorMsg
          [r| table T { x: string = 0;   } |] `shouldFail` errorMsg
          [r| table T { x: string = 0.0; } |] `shouldFail` errorMsg

      describe "with reference to enum" $ do
        it "simple" $
          [r|
            namespace A.B;
            enum E : short { A }
            table T {
              x: B.E;
            }
          |] `shouldValidate` foldDecls
            [ enum ("A.B", EnumDecl "E" EInt16 [ EnumVal "A" 0 ])
            , table ("A.B", TableDecl "T"
                [ TableField "x" (TEnum (TypeRef "A.B" "E") "A") False ]
              )
            ]

        it "with `required` attribute" $
          [r| table T { x: E (required); } enum E : short{A} |] `shouldFail`
            "[T.x]: only non-scalar fields (strings, vectors, unions, structs, tables) may be 'required'"

        it "with `deprecated` attribute" $
          [r| table T { x: E (deprecated); } enum E : short{A} |] `shouldValidate` foldDecls
            [ enum ("", EnumDecl "E" EInt16 [ EnumVal "A" 0 ])
            , table ("", TableDecl "T"
                [ TableField "x" (TEnum (TypeRef "" "E") "A") True ]
              )
            ]

        it "without default value, when enum has 0-value" $
          [r| table T { x: E; } enum E : short{ A = -1, B = 0, C = 1} |] `shouldValidate` foldDecls
            [ enum ("", EnumDecl "E" EInt16 [ EnumVal "A" (-1), EnumVal "B" 0, EnumVal "C" 1 ])
            , table ("", TableDecl "T"
                [ TableField "x" (TEnum (TypeRef "" "E") "B") False ]
              )
            ]

        it "without default value, when enum doesn't have 0-value" $
          [r| table T { x: E; } enum E : short{ A = -1, B = 1, C = 2} |] `shouldFail`
            "[T.x]: enum does not have a 0 value; please manually specify a default for this field"

        describe "with default value" $ do
          it "valid integral" $
            [r| table T { x: E = 1; } enum E : short{ A, B, C } |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EInt16 [ EnumVal "A" 0, EnumVal "B" 1, EnumVal "C" 2 ])
              , table ("", TableDecl "T"
                  [ TableField "x" (TEnum (TypeRef "" "E") "B") False ]
                )
              ]

          it "invalid integral" $
            [r| table T { x: E = 3; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value of 3 is not part of enum E"

          it "valid identifier" $
            [r| table T { x: E = B; } enum E : short{ A, B, C } |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EInt16 [ EnumVal "A" 0, EnumVal "B" 1, EnumVal "C" 2 ])
              , table ("", TableDecl "T"
                  [ TableField "x" (TEnum (TypeRef "" "E") "B") False ]
                )
              ]

          it "invalid identifier" $
            [r| table T { x: E = D; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value of D is not part of enum E"

          it "decimal number" $
            [r| table T { x: E = 1.5; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value must be integral or ['A', 'B', 'C']"

          it "boolean" $
            [r| table T { x: E = 1.5; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value must be integral or ['A', 'B', 'C']"

      describe "with reference to structs/table/union" $ do
        it "simple" $
          [r|
            namespace A;
            struct S { x: int; }
            table T {}
            union U {A.T}
            table Table {
              x: S;
              y: T;
              z: U;
            }
          |] `shouldValidate` foldDecls
            [ struct ("A", StructDecl "S" 4 [ StructField "x" 0 SInt32 ])
            , table ("A", TableDecl "T" [])
            , union ("A", ST.UnionDecl "U" (ST.Metadata []) [ST.UnionVal Nothing (TypeRef "A" "T")])
            , table ("A", TableDecl "Table"
                [ TableField "x" (TStruct (TypeRef "A" "S") Opt) False
                , TableField "y" (TTable (TypeRef "A" "T") Opt) False
                , TableField "z" (TUnion (TypeRef "A" "U") Opt) False
                ]
              )
            ]

        it "with `required` attribute" $
          [r|
            namespace A;
            struct S { x: int; }
            table T {}
            union U {A.T}
            table Table {
              x: S (required);
              y: T (required);
              z: U (required);
            }
          |] `shouldValidate` foldDecls
            [ struct ("A", StructDecl "S" 4 [ StructField "x" 0 SInt32 ])
            , table ("A", TableDecl "T" [])
            , union ("A", ST.UnionDecl "U" (ST.Metadata []) [ST.UnionVal Nothing (TypeRef "A" "T")])
            , table ("A", TableDecl "Table"
                [ TableField "x" (TStruct (TypeRef "A" "S") Req) False
                , TableField "y" (TTable (TypeRef "A" "T") Req) False
                , TableField "z" (TUnion (TypeRef "A" "U") Req) False
                ]
              )
            ]

        it "with `deprecated` attribute" $
          [r|
            namespace A;
            struct S { x: int; }
            table T {}
            union U {A.T}
            table Table {
              x: S (deprecated);
              y: T (deprecated);
              z: U (deprecated);
            }
          |] `shouldValidate` foldDecls
            [ struct ("A", StructDecl "S" 4 [ StructField "x" 0 SInt32 ])
            , table ("A", TableDecl "T" [])
            , union ("A", ST.UnionDecl "U" (ST.Metadata []) [ST.UnionVal Nothing (TypeRef "A" "T")])
            , table ("A", TableDecl "Table"
                [ TableField "x" (TStruct (TypeRef "A" "S") Opt) True
                , TableField "y" (TTable (TypeRef "A" "T") Opt) True
                , TableField "z" (TUnion (TypeRef "A" "U") Opt) True
                ]
              )
            ]

        it "with default value" $ do
          let errorMsg = "[Table.x]: default values currently only supported for scalar fields (integers, floating point, bool, enums)"
          [r| table Table { x: S = 0; }   struct S { x: int; } |] `shouldFail` errorMsg
          [r| table Table { x: T = 0; }   table T{}            |] `shouldFail` errorMsg
          [r| table Table { x: U = 0; }   union U{T}           |] `shouldFail` errorMsg

      it "with invalid reference" $ do
        [r| table T { x: A.X; }   |] `shouldFail` "[T.x]: type 'A.X' does not exist (checked in these namespaces: [''])"
        [r| table T { x: [A.X]; } |] `shouldFail` "[T.x]: type 'A.X' does not exist (checked in these namespaces: [''])"

      it "with duplicate fields" $
        [r| table T { x: byte; x: int; } |] `shouldFail`
          "[T]: [x] declared more than once"

      describe "with `id` attribute" $ do
        it "sorts fields" $
          [r|
            table T {
              w: byte (id: 2);
              x: byte (id: 1);
              y: byte (id: 3);
              z: byte (id: 0);
            }
          |] `shouldValidate` foldDecls
            [ table ("", TableDecl "T"
                [ TableField "z" (TInt8 0) False
                , TableField "x" (TInt8 0) False
                , TableField "w" (TInt8 0) False
                , TableField "y" (TInt8 0) False
                ]
              )
            ]

        it "id can be a string, if it's coercible to an integer" $
          [r|
            table T {
              x: byte (id: "1");
              y: byte (id: "  02 ");
              z: byte (id: "0");
            }
          |] `shouldValidate` foldDecls
            [ table ("", TableDecl "T"
                [ TableField "z" (TInt8 0) False
                , TableField "x" (TInt8 0) False
                , TableField "y" (TInt8 0) False
                ]
              )
            ]

        it "ids cannot be non-integral string" $ do
          [r| table T { x: byte (id: "0.3");   } |] `shouldFail` "[T]: expected attribute 'id' to have an integer value, e.g. 'id: 123'"
          [r| table T { x: byte (id: "hello"); } |] `shouldFail` "[T]: expected attribute 'id' to have an integer value, e.g. 'id: 123'"

        it "when one field has it, all other fields must have it as well" $
          [r| table T { x: byte (id: 0); y: int; } |] `shouldFail`
            "[T]: either all fields or no fields must have an 'id' attribute"

        it "ids must be consecutive" $
          [r| table T { x: byte (id: 0); y: int (id: 2); } |] `shouldFail`
            "[T]: field ids must be consecutive from 0"

        it "ids must start from 0" $
         [r| table T { x: byte (id: 1); y: int (id: 2); } |] `shouldFail`
            "[T]: field ids must be consecutive from 0"


foldDecls :: [Stage4] -> Stage4
foldDecls = fold

enum :: (Namespace, EnumDecl) -> Stage4
enum e = SymbolTable [e] [] [] []

struct :: (Namespace, StructDecl) -> Stage4
struct s = SymbolTable [] [s] [] []

table :: (Namespace, TableDecl) -> Stage4
table t = SymbolTable [] [] [t] []

union :: (Namespace, ST.UnionDecl) -> Stage4
union u = SymbolTable [] [] [] [u]

shouldValidate :: String -> Stage4 -> Expectation
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
