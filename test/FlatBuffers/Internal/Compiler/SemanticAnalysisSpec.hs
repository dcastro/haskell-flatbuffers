{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FlatBuffers.Internal.Compiler.SemanticAnalysisSpec where

import           Data.Bits                                      ( shiftL )
import           Data.Foldable                                  ( fold )
import           Data.Int

import qualified FlatBuffers.Internal.Compiler.Parser           as P
import           FlatBuffers.Internal.Compiler.SemanticAnalysis
import           FlatBuffers.Internal.Compiler.SyntaxTree       ( FileTree(..) )
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree

import           TestImports

import           Text.Megaparsec
import           Text.RawString.QQ                              ( r )


spec :: Spec
spec =
  describe "SemanticAnalysis" $ do
    it "top-level identifiers cannot have duplicates in the same namespace" $ do
      [r| namespace A; enum E:int{x} enum E:int{x} |] `shouldFail` "'A.E' declared more than once"
      [r| enum E:int{x}     enum E:int{x}     |] `shouldFail` "'E' declared more than once"
      [r| struct S{x:int;}  struct S{x:int;}  |] `shouldFail` "'S' declared more than once"
      [r| table T{}         table T{}         |] `shouldFail` "'T' declared more than once"
      [r| union U{x}        union U{x}        |] `shouldFail` "'U' declared more than once"
      [r| union U{x}        union U{x}        |] `shouldFail` "'U' declared more than once"
      [r| union X{x}        table X{}         |] `shouldFail` "'X' declared more than once"

    it "top-level identifiers can be duplicates, if they live in different namespaces" $
      [r|
        namespace A;
        union X{B.X}

        namespace B;
        table X{}
      |] `shouldValidate` foldDecls
        [ union ("A", UnionDecl "X" [UnionVal "B_X" (TypeRef "B" "X")])
        , table ("B", TableDecl "X" NotRoot [])
        ]

    describe "attributes" $ do
      it "user defined attributes must be declared" $ do
        [r| enum E : int (x) {Y}      |] `shouldFail` "[E]: user defined attributes must be declared before use: x"
        [r| struct S (x) { y: int;}   |] `shouldFail` "[S]: user defined attributes must be declared before use: x"
        [r| struct S { y: int (x);}   |] `shouldFail` "[S.y]: user defined attributes must be declared before use: x"
        [r| table T (x) {}            |] `shouldFail` "[T]: user defined attributes must be declared before use: x"
        [r| table T { y: int (x); }   |] `shouldFail` "[T.y]: user defined attributes must be declared before use: x"
        [r| union U (x) {Y} table Y{} |] `shouldFail` "[U]: user defined attributes must be declared before use: x"

      it "user defined attributes can be used when declared" $ do
        shouldSucceed [r| attribute x; enum E : int (x) {Y}      |]
        shouldSucceed [r| attribute x; struct S (x) { y: int;}   |]
        shouldSucceed [r| attribute x; struct S { y: int (x);}   |]
        shouldSucceed [r| attribute x; table T (x) {}            |]
        shouldSucceed [r| attribute x; table T { y: int (x); }   |]
        shouldSucceed [r| attribute x; union U (x) {Y} table Y{} |]

      it "built-in attributes can be used without being declared" $
        shouldSucceed
          [r|
            table T
              (id,
              deprecated,
              required,
              force_align,
              bit_flags,
              nested_flatbuffer,
              flexbuffer,
              key,
              hash,
              original_order,
              native_inline,
              native_default,
              native_custom_alloc,
              native_type
             ) {}
          |]

    describe "root type" $ do
      it "flips the `isRoot` flag" $
        [r|
          table T{}
          root_type T;
        |] `shouldValidate`
          table ("", TableDecl "T" (IsRoot Nothing) [])

      it "can be paired with a file_identifier" $
        [r|
          table T{}
          file_identifier "abcd";
          root_type T;
        |] `shouldValidate`
          table ("", TableDecl "T" (IsRoot (Just "abcd")) [])

      it "when set multiple times, the last declaration wins" $
        [r|
          file_identifier "abcd";
          root_type T;
          root_type T2;
          file_identifier "efgh";
          table T2{}
          table T{}
        |] `shouldValidate` foldDecls
          [ table ("", TableDecl "T2" (IsRoot (Just "efgh")) [])
          , table ("", TableDecl "T" NotRoot [])
          ]

      it "must reference a table" $ do
        [r| root_type E; enum E:int{x}        |] `shouldFail` "root type must be a table"
        [r| root_type S; struct S{x:int;}     |] `shouldFail` "root type must be a table"
        [r| root_type U; union U{T} table T{} |] `shouldFail` "root type must be a table"
        [r| root_type string;                 |] `shouldFail` "type 'string' does not exist (checked in these namespaces: '')"

      it "can reference tables in other namespaces" $
        [r|
          namespace A;
          root_type B.T;
          namespace A.B;
          table T{}
          file_identifier "abcd";
        |] `shouldValidate`
          table ("A.B", TableDecl "T" (IsRoot (Just "abcd")) [])

      it "a file identifier on its own doesn't do anything" $
        [r|
          table T{}
          file_identifier "abcd";
        |] `shouldValidate`
          table ("", TableDecl "T" NotRoot [])

    describe "enums" $ do
      it "simple" $
        [r|
          namespace Ns;
          enum Color : uint32 { Red, Green, Blue }
        |] `shouldValidate`
          enum ("Ns", EnumDecl "Color" EWord32 False
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
          [ enum ("A",      EnumDecl "Color1" EWord32 False [EnumVal "Red" 0] )
          , enum ("",       EnumDecl "Color2" EWord32 False [EnumVal "Green" 0] )
          , enum ("A.B.C",  EnumDecl "Color3" EWord32 False [EnumVal "Blue" 0] )
          ]

      it "with explicit values" $
        [r| enum Color : int32 { Red = -2, Green, Blue = 2 } |] `shouldValidate`
          enum ("", EnumDecl "Color" EInt32 False
            [ EnumVal "Red" (-2)
            , EnumVal "Green" (-1)
            , EnumVal "Blue" 2
            ])

      it "with explicit values (min/maxBound)" $
        [r| enum Color : int8 { Red = -128, Green, Blue = 127 } |] `shouldValidate`
          enum ("", EnumDecl "Color" EInt8 False
          [ EnumVal "Red" (toInteger (minBound :: Int8))
          , EnumVal "Green" (-127)
          , EnumVal "Blue" (toInteger (maxBound :: Int8))
          ])

      it "with out-of-bounds values" $ do
        [r|
          namespace A.B;
          enum Color : int8 { Red = -129, Green, Blue }
        |] `shouldFail`
          "[A.B.Color.Red]: enum value of -129 does not fit [-128; 127]"
        [r|
          enum Color : int8 { Red, Green, Blue = 128 }
        |] `shouldFail`
          "[Color.Blue]: enum value of 128 does not fit [-128; 127]"

      it "with values out of order" $ do
        [r| enum Color : int8 { Red = 3, Green = 2, Blue } |] `shouldFail`
          "[Color]: enum values must be specified in ascending order. 'Green' (2) should be greater than 'Red' (3)"
        [r| enum Color : int8 { Red = 3, Green = 3, Blue } |] `shouldFail`
          "[Color]: enum values must be specified in ascending order. 'Green' (3) should be greater than 'Red' (3)"

      it "with duplicate values" $
        [r| enum Color : int8 { Red, Green, Red, Gray, Green, Green, Black } |] `shouldFail`
          "[Color]: 'Green', 'Red' declared more than once"

      it "with invalid underlying type" $ do
        let expected = "[Color]: underlying enum type must be integral"
        [r| enum Color : double  { Red, Green, Blue } |] `shouldFail` expected
        [r| enum Color : TypeRef { Red, Green, Blue } |] `shouldFail` expected
        [r| enum Color : [int]   { Red, Green, Blue } |] `shouldFail` expected

    describe "enums with bit_flags" $ do
      it "simple" $
        [r|
          namespace Ns;
          enum Color : uint32 (bit_flags) { Red, Green, Blue }
        |] `shouldValidate`
          enum ("Ns", EnumDecl "Color" EWord32 True
            [ EnumVal "Red" 1
            , EnumVal "Green" 2
            , EnumVal "Blue" 4
            ])

      it "multiple enums in different namespaces" $
        [r|
          namespace A;
          enum Color1 : uint32 (bit_flags) { Red }

          namespace B;
          namespace ;
          enum Color2 : uint32 (bit_flags) { Green }

          namespace A.B.C;
          enum Color3 : uint32 (bit_flags) { Blue }

        |] `shouldValidate` foldDecls
          [ enum ("A",      EnumDecl "Color1" EWord32 True [EnumVal "Red" 1] )
          , enum ("",       EnumDecl "Color2" EWord32 True [EnumVal "Green" 1] )
          , enum ("A.B.C",  EnumDecl "Color3" EWord32 True [EnumVal "Blue" 1] )
          ]

      it "with explicit values" $
        [r| enum Color : uint8 (bit_flags) { Red = 2, Green, Blue = 6 } |] `shouldValidate`
          enum ("", EnumDecl "Color" EWord8 True
            [ EnumVal "Red" 4
            , EnumVal "Green" 8
            , EnumVal "Blue" 64
            ])

      it "with explicit values (min/maxBound)" $
        [r| enum Color : uint (bit_flags) { Red = 0, Green, Blue = 31 } |] `shouldValidate`
          enum ("", EnumDecl "Color" EWord32 True
          [ EnumVal "Red" 1
          , EnumVal "Green" 2
          , EnumVal "Blue" (1 `shiftL` 31)
          ])

      it "with out-of-bounds values" $ do
        [r|
          namespace A.B;
          enum Color : uint (bit_flags) { Red = -1, Green, Blue }
        |] `shouldFail`
          "[A.B.Color.Red]: enum value of -1 does not fit [0; 31]"
        [r|
          enum Color : uint (bit_flags) { Red, Green, Blue = 32 }
        |] `shouldFail`
          "[Color.Blue]: enum value of 32 does not fit [0; 31]"

      it "with values out of order" $ do
        [r| enum Color : uint8 (bit_flags) { Red = 3, Green = 2, Blue } |] `shouldFail`
          "[Color]: enum values must be specified in ascending order. 'Green' (2) should be greater than 'Red' (3)"
        [r| enum Color : uint8 (bit_flags) { Red = 3, Green = 3, Blue } |] `shouldFail`
          "[Color]: enum values must be specified in ascending order. 'Green' (3) should be greater than 'Red' (3)"

      it "with duplicate values" $
        [r| enum Color : uint8 (bit_flags) { Red, Green, Red, Gray, Green, Green, Black } |] `shouldFail`
          "[Color]: 'Green', 'Red' declared more than once"

      it "with invalid underlying type" $ do
        let expected = "[Color]: underlying enum type must be integral"
        [r| enum Color : double  (bit_flags) { Red, Green, Blue } |] `shouldFail` expected
        [r| enum Color : TypeRef (bit_flags) { Red, Green, Blue } |] `shouldFail` expected
        [r| enum Color : [int]   (bit_flags) { Red, Green, Blue } |] `shouldFail` expected

      it "with signed underlying type" $ do
        let expected = "[Color]: underlying type of bit_flags enum must be unsigned"
        [r| enum Color : int8  (bit_flags) { Red, Green, Blue } |] `shouldFail` expected
        [r| enum Color : int16 (bit_flags) { Red, Green, Blue } |] `shouldFail` expected
        [r| enum Color : int32 (bit_flags) { Red, Green, Blue } |] `shouldFail` expected
        [r| enum Color : int64 (bit_flags) { Red, Green, Blue } |] `shouldFail` expected

    describe "structs" $ do
      it "simple" $
        [r|
          namespace Ns;
          struct S {
            x: int;
          }
        |] `shouldValidate`
          struct ("Ns", StructDecl "S" 4 4
            [ StructField "x" 0 0 SInt32
            ])

      it "multiple fields" $
        [r|
          struct S {
            x: ubyte;
            y: double;
            z: bool;
          }
        |] `shouldValidate`
          struct ("", StructDecl "S" 8 24
            [ StructField "x" 7 0 SWord8
            , StructField "y" 0 8 SDouble
            , StructField "z" 7 16 SBool
            ])

      it "when unqualified TypeRef is ambiguous, types in namespaces closer to the struct are preferred" $ do
        let enumVal = EnumVal "x" 0
            mkEnum namespace ident = enum (namespace, EnumDecl ident EInt16 False [enumVal])
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
          , struct ("A.B", StructDecl "S" 2 6
              [ StructField "x" 0 0 (SEnum (TypeRef "A.B"  "E1") EInt16)
              , StructField "y" 0 2 (SEnum (TypeRef "A"    "E2") EInt16)
              , StructField "z" 0 4 (SEnum (TypeRef ""     "E3") EInt16)
              ])
          ]

      it "when qualified TypeRef is ambiguous, types in namespaces closer to the struct are preferred" $ do
        let enumVal = EnumVal "x" 0
            mkEnum namespace ident = enum (namespace, EnumDecl ident EInt16 False [enumVal])
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
          , struct ("A.B", StructDecl "S" 2 6
              [ StructField "x" 0 0 (SEnum (TypeRef "A.B.A" "E1") EInt16)
              , StructField "y" 0 2 (SEnum (TypeRef "A.A"   "E2") EInt16)
              , StructField "z" 0 4 (SEnum (TypeRef "A"     "E3") EInt16)
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
          [ enum ("A", EnumDecl "Color" EWord16 False [EnumVal "Blue" 0])
          , struct ("A", StructDecl "S" 2 2
              [ StructField "x" 0 0 (SEnum (TypeRef "A" "Color") EWord16)
              ])
          ]


      it "with field referencing an enum with bit_flags" $
        [r|
          namespace A;
          enum Color : ushort (bit_flags) { Blue }

          struct S {
            x: Color;
          }
        |] `shouldValidate` foldDecls
          [ enum ("A", EnumDecl "Color" EWord16 True [EnumVal "Blue" 1])
          , struct ("A", StructDecl "S" 2 2
              [ StructField "x" 0 0 (SEnum (TypeRef "A" "Color") EWord16)
              ])
          ]

      it "with nested structs (backwards/forwards references)" $ do
        let backwards = ("A.B", StructDecl "Backwards" 4 4 [ StructField "x" 0 0 SFloat ])
        let forwards  = ("A.B", StructDecl "Forwards"  4 4 [ StructField "y" 0 0 (SStruct backwards) ])
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
          [ struct backwards
          , struct ("A.B", StructDecl "S" 4 8
              [ StructField "x1" 0 0 (SStruct backwards)
              , StructField "x2" 0 4 (SStruct forwards)
              ])
          , struct forwards
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
          "[X.Y.Z.S.x]: type 'A.T' does not exist (checked in these namespaces: 'X.Y.Z', 'X.Y', 'X', '')"

      it "with reference to a vector" $
        [r| struct S { x: [byte]; } |] `shouldFail`
         "[S.x]: struct fields may only be integers, floating point, bool, enums, or other structs"

      it "with reference to a string" $
        [r| struct S { x: string; } |] `shouldFail`
          "[S.x]: struct fields may only be integers, floating point, bool, enums, or other structs"

      it "with duplicate fields" $
        [r| struct S { x: byte; x: int; } |] `shouldFail`
          "[S]: 'x' declared more than once"

      it "with `force_align` attribute" $ do
        -- just 1 field
        [r| struct S (force_align: 4)  { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 4  4  [StructField "x" 0 0 SInt32])
        [r| struct S (force_align: 8)  { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 8  8  [StructField "x" 4 0 SInt32])
        [r| struct S (force_align: 16) { x: int; } |] `shouldValidate` struct ("", StructDecl "S" 16 16 [StructField "x" 12 0 SInt32])
        -- multiple fields
        [r| struct S (force_align: 2)  { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 2  4  [StructField "x" 1 0 SInt8, StructField "y" 0 2 SWord16])
        [r| struct S (force_align: 4)  { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 4  4  [StructField "x" 1 0 SInt8, StructField "y" 0 2 SWord16])
        [r| struct S (force_align: 8)  { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 8  8  [StructField "x" 1 0 SInt8, StructField "y" 4 2 SWord16])
        [r| struct S (force_align: 16) { x: byte; y: ushort; } |] `shouldValidate` struct ("", StructDecl "S" 16 16 [StructField "x" 1 0 SInt8, StructField "y" 12 2 SWord16])
        -- nested structs
        let s1 = ("", StructDecl "S1" 2 2 [StructField "x" 1 0 SInt8])
        let s2 = ("", StructDecl "S2" 4 4 [StructField "x" 0 0 SInt32])
        let s  = ("", StructDecl "S" 4 12
                  [ StructField "x" 2 0 (SStruct s1)
                  , StructField "y" 0 4 (SStruct s2)
                  , StructField "z" 3 8 SBool
                  ])
        [r|
          struct S (force_align: 4) { x: S1; y: S2; z: bool; }
          struct S1 (force_align: 2) { x: byte; }
          struct S2 { x: int; }
        |] `shouldValidate` foldDecls
          [ struct s
          , struct s1
          , struct s2
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

      it "with required field" $
        [r| struct S { x: byte (required); } |] `shouldFail`
          "[S.x]: struct fields are already required, the 'required' attribute is redundant"

      it "with id field" $
        [r| struct S { x: byte (id: 0); } |] `shouldFail`
          "[S.x]: struct fields cannot be reordered using the 'id' attribute"

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
        [r| table T{} |] `shouldValidate` table ("", TableDecl "T" NotRoot [])

      it "with cyclic reference" $
        [r| table T{x: T;} |] `shouldValidate`
          table ("", TableDecl "T" NotRoot
            [ TableField 0 "x" (TTable (TypeRef "" "T") Opt) False
            ])

      it "with invalid reference" $ do
        [r| table T { x: A.X; }   |] `shouldFail` "[T.x]: type 'A.X' does not exist (checked in these namespaces: '')"
        [r| table T { x: [A.X]; } |] `shouldFail` "[T.x]: type 'A.X' does not exist (checked in these namespaces: '')"

      it "with duplicate fields" $
        [r| table T { x: byte; x: int; } |] `shouldFail`
          "[T]: 'x' declared more than once"

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
            table ("A.B", TableDecl "T" NotRoot
              [ TableField 0 "a" (TInt8 0) False
              , TableField 1 "b" (TInt16 0) False
              , TableField 2 "c" (TInt32 0) False
              , TableField 3 "d" (TInt64 0) False
              , TableField 4 "e" (TWord8 0) False
              , TableField 5 "f" (TWord16 0) False
              , TableField 6 "g" (TWord32 0) False
              , TableField 7 "h" (TWord64 0) False
              , TableField 8 "i" (TFloat 0) False
              , TableField 9 "j" (TDouble 0) False
              , TableField 10"k" (TBool (DefaultVal False)) False
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
            table ("A.B", TableDecl "T" NotRoot
              [ TableField 0 "a" (TInt8 0) True
              , TableField 1 "b" (TInt16 0) True
              , TableField 2 "c" (TInt32 0) True
              , TableField 3 "d" (TInt64 0) True
              , TableField 4 "e" (TWord8 0) True
              , TableField 5 "f" (TWord16 0) True
              , TableField 6 "g" (TWord32 0) True
              , TableField 7 "h" (TWord64 0) True
              , TableField 8 "i" (TFloat 0) True
              , TableField 9 "j" (TDouble 0) True
              , TableField 10 "k" (TBool (DefaultVal False)) True
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
            table ("", TableDecl "T" NotRoot
              [ TableField 0 "a" (TInt8 127) False
              , TableField 1 "b" (TInt16 (-32768)) False
              , TableField 2 "c" (TInt32 1) False
              , TableField 3 "d" (TInt64 1) False
              , TableField 4 "e" (TWord8 20) False
              , TableField 5 "f" (TWord16 20) False
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
            table ("", TableDecl "T" NotRoot
              [ TableField 0 "a" (TFloat 127) False
              , TableField 1 "b" (TFloat (-32768)) False
              , TableField 2 "c" (TFloat 1) False
              , TableField 3 "d" (TDouble 1) False
              , TableField 4 "e" (TDouble 20) False
              , TableField 5 "f" (TDouble 20) False
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
            table ("", TableDecl "T" NotRoot
              [ TableField 0 "a" (TDouble 1.1) False
              , TableField 1 "b" (TDouble 0.2) False
              , TableField 2 "c" (TDouble 22.2) False
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
            table ("", TableDecl "T" NotRoot
              [ TableField 0 "a" (TBool (DefaultVal True)) False
              , TableField 1 "b" (TBool (DefaultVal False)) False
              ]
            )

        it "with identifier default values" $
          [r| table T { a: bool = Red; } |] `shouldFail` "[T.a]: default value must be a boolean"

      describe "with string fields" $ do
        it "simple" $
          [r| table T { x: string; } |] `shouldValidate`
            table ("", TableDecl "T" NotRoot [ TableField 0 "x" (TString Opt) False ])
        it "with `required` attribute" $
          [r| table T { x: string (required); } |] `shouldValidate`
            table ("", TableDecl "T" NotRoot [ TableField 0 "x" (TString Req) False ])
        it "with `deprecated` attribute" $
          [r| table T { x: string (deprecated); } |] `shouldValidate`
            table ("", TableDecl "T" NotRoot [ TableField 0 "x" (TString Opt) True ])
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
            [ enum ("A.B", EnumDecl "E" EInt16 False [ EnumVal "A" 0 ])
            , table ("A.B", TableDecl "T" NotRoot
                [ TableField 0 "x" (TEnum (TypeRef "A.B" "E") EInt16 0) False ]
              )
            ]

        it "with `required` attribute" $
          [r|
            table T { x: E (required); }
            enum E : short{A}
          |] `shouldFail`
            "[T.x]: only non-scalar fields (strings, vectors, unions, structs, tables) may be 'required'"

        it "with `deprecated` attribute" $
          [r|
            table T { x: E (deprecated); }
            enum E : short{A}
          |] `shouldValidate` foldDecls
            [ enum ("", EnumDecl "E" EInt16 False [ EnumVal "A" 0 ])
            , table ("", TableDecl "T" NotRoot
                [ TableField 0 "x" (TEnum (TypeRef "" "E") EInt16 0) True ]
              )
            ]

        it "without default value, when enum has 0-value" $
          [r|
            table T { x: E; }
            enum E : short{ A = -1, B = 0, C = 1}
          |] `shouldValidate` foldDecls
            [ enum ("", EnumDecl "E" EInt16 False [ EnumVal "A" (-1), EnumVal "B" 0, EnumVal "C" 1 ])
            , table ("", TableDecl "T" NotRoot
                [ TableField 0 "x" (TEnum (TypeRef "" "E") EInt16 0) False ]
              )
            ]

        it "without default value, when enum doesn't have 0-value" $
          [r|
            table T { x: E; }
            enum E : short{ A = -1, B = 1, C = 2}
          |] `shouldFail`
            "[T.x]: enum does not have a 0 value; please manually specify a default for this field"

        describe "with default value" $ do
          it "valid integral" $
            [r|
              table T { x: E = 1; }
              enum E : short{ A, B, C }
            |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EInt16 False [ EnumVal "A" 0, EnumVal "B" 1, EnumVal "C" 2 ])
              , table ("", TableDecl "T" NotRoot
                  [ TableField 0 "x" (TEnum (TypeRef "" "E") EInt16 1) False ]
                )
              ]

          it "integral must match one of the enum values" $
            [r|
              table T { x: E = 3; }
              enum E : short{ A, B, C }
            |] `shouldFail`
              "[T.x]: default value of 3 is not part of enum E"

          it "valid identifier" $
            [r|
              table T { x: E = B; }
              enum E : short{ A, B, C }
            |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EInt16 False [ EnumVal "A" 0, EnumVal "B" 1, EnumVal "C" 2 ])
              , table ("", TableDecl "T" NotRoot
                  [ TableField 0 "x" (TEnum (TypeRef "" "E") EInt16 1) False ]
                )
              ]

          it "invalid identifier" $
            [r| table T { x: E = D; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value of D is not part of enum E"

          it "multiple identifiers" $
            [r| table T { x: E = "B C"; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value must be a single identifier, found 2: 'B', 'C'"

          it "decimal number" $
            [r| table T { x: E = 1.5; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value must be integral or one of: 'A', 'B', 'C'"

          it "boolean" $
            [r| table T { x: E = true; } enum E : short{ A, B, C } |] `shouldFail`
              "[T.x]: default value must be integral or one of: 'A', 'B', 'C'"

      describe "with reference to enum with bit_flags" $ do
        it "simple" $
          [r|
            namespace A.B;
            enum E : ushort (bit_flags) { A }
            table T {
              x: B.E;
            }
          |] `shouldValidate` foldDecls
            [ enum ("A.B", EnumDecl "E" EWord16 True [ EnumVal "A" 1 ])
            , table ("A.B", TableDecl "T" NotRoot
                [ TableField 0 "x" (TEnum (TypeRef "A.B" "E") EWord16 0) False ]
              )
            ]

        it "with `required` attribute" $
          [r|
            table T { x: E (required); }
            enum E : ushort (bit_flags) {A}
          |] `shouldFail`
            "[T.x]: only non-scalar fields (strings, vectors, unions, structs, tables) may be 'required'"

        it "with `deprecated` attribute" $
          [r|
            table T { x: E (deprecated); }
            enum E : ushort (bit_flags) {A}
          |] `shouldValidate` foldDecls
            [ enum ("", EnumDecl "E" EWord16 True [ EnumVal "A" 1 ])
            , table ("", TableDecl "T" NotRoot
                [ TableField 0 "x" (TEnum (TypeRef "" "E") EWord16 0) True ]
              )
            ]

        it "without default value, when enum doesn't have 1-value" $
          [r|
            table T { x: E; }
            enum E : ushort (bit_flags) { A = 9 }
          |] `shouldValidate` foldDecls
            [ enum ("", EnumDecl "E" EWord16 True [ EnumVal "A" 512 ])
            , table ("", TableDecl "T" NotRoot
                [ TableField 0 "x" (TEnum (TypeRef "" "E") EWord16 0) False ]
              )
            ]

        describe "with default value" $ do
          it "valid integral" $
            [r|
              table T { x: E = 2; }
              enum E : ushort (bit_flags) { A, B, C }
            |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EWord16 True [ EnumVal "A" 1, EnumVal "B" 2, EnumVal "C" 4 ])
              , table ("", TableDecl "T" NotRoot
                  [ TableField 0 "x" (TEnum (TypeRef "" "E") EWord16 2) False ]
                )
              ]

          it "integral doesn't have to match any enum value" $
            [r|
              table T { x: E = 65535; }
              enum E : ushort (bit_flags) { A, B, C }
            |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EWord16 True [ EnumVal "A" 1, EnumVal "B" 2, EnumVal "C" 4 ])
              , table ("", TableDecl "T" NotRoot
                  [ TableField 0 "x" (TEnum (TypeRef "" "E") EWord16 65535) False ]
                )
              ]

          it "must be within the range of the enum's underlying type" $
            [r|
              table T { x: E = 65536; }
              enum E : ushort (bit_flags) { A, B, C }
            |] `shouldFail`
              "[T.x]: default value does not fit [0; 65535]"

          it "valid identifier" $
            [r|
              table T { x: E = B; }
              enum E : ushort (bit_flags) { A, B, C }
            |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EWord16 True [ EnumVal "A" 1, EnumVal "B" 2, EnumVal "C" 4 ])
              , table ("", TableDecl "T" NotRoot
                  [ TableField 0 "x" (TEnum (TypeRef "" "E") EWord16 2) False ]
                )
              ]

          it "invalid identifier" $
            [r| table T { x: E = D; } enum E : ushort (bit_flags) { A, B, C } |] `shouldFail`
              "[T.x]: default value of D is not part of enum E"

          it "multiple valid identifiers" $
            [r|
              table T { x: E = "B C"; }
              enum E : ushort (bit_flags) { A, B, C }
            |] `shouldValidate` foldDecls
              [ enum ("", EnumDecl "E" EWord16 True [ EnumVal "A" 1, EnumVal "B" 2, EnumVal "C" 4 ])
              , table ("", TableDecl "T" NotRoot
                  [ TableField 0 "x" (TEnum (TypeRef "" "E") EWord16 6) False ]
                )
              ]

          it "mix of valid and invalid identifiers" $
            [r|
              table T { x: E = "B X C"; }
              enum E : ushort (bit_flags) { A, B, C }
            |] `shouldFail`
              "[T.x]: default value of X is not part of enum E"

          it "decimal number" $
            [r| table T { x: E = 1.5; } enum E : ushort (bit_flags) { A, B, C } |] `shouldFail`
              "[T.x]: default value must be integral, one of ['A', 'B', 'C'], or a combination of the latter in double quotes (e.g. \"A B\")"

          it "boolean" $
            [r| table T { x: E = true; } enum E : ushort (bit_flags) { A, B, C } |] `shouldFail`
              "[T.x]: default value must be integral, one of ['A', 'B', 'C'], or a combination of the latter in double quotes (e.g. \"A B\")"

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
            [ struct ("A", StructDecl "S" 4 4 [ StructField "x" 0 0 SInt32 ])
            , table ("A", TableDecl "T" NotRoot [])
            , union ("A", UnionDecl "U" [UnionVal "A_T" (TypeRef "A" "T")])
            , table ("A", TableDecl "Table" NotRoot
                [ TableField 0 "x" (TStruct (TypeRef "A" "S") Opt) False
                , TableField 1 "y" (TTable (TypeRef "A" "T") Opt) False
                , TableField 3 "z" (TUnion (TypeRef "A" "U") Opt) False
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
            [ struct ("A", StructDecl "S" 4 4 [ StructField "x" 0 0 SInt32 ])
            , table ("A", TableDecl "T" NotRoot [])
            , union ("A", UnionDecl "U" [UnionVal "A_T" (TypeRef "A" "T")])
            , table ("A", TableDecl "Table" NotRoot
                [ TableField 0 "x" (TStruct (TypeRef "A" "S") Req) False
                , TableField 1 "y" (TTable (TypeRef "A" "T") Req) False
                , TableField 3 "z" (TUnion (TypeRef "A" "U") Req) False
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
            [ struct ("A", StructDecl "S" 4 4 [ StructField "x" 0 0 SInt32 ])
            , table ("A", TableDecl "T" NotRoot [])
            , union ("A", UnionDecl "U" [UnionVal "A_T" (TypeRef "A" "T")])
            , table ("A", TableDecl "Table" NotRoot
                [ TableField 0 "x" (TStruct (TypeRef "A" "S") Opt) True
                , TableField 1 "y" (TTable (TypeRef "A" "T") Opt) True
                , TableField 3 "z" (TUnion (TypeRef "A" "U") Opt) True
                ]
              )
            ]

        it "with default value" $ do
          let errorMsg = "[Table.x]: default values currently only supported for scalar fields (integers, floating point, bool, enums)"
          [r| table Table { x: S = 0; }   struct S { x: int; } |] `shouldFail` errorMsg
          [r| table Table { x: T = 0; }   table T{}            |] `shouldFail` errorMsg
          [r| table Table { x: U = 0; }   union U{T}           |] `shouldFail` errorMsg

      describe "with vector fields" $ do
        it "simple" $
          [r| table T { x: [string]; y: [int]; z: [bool]; } |] `shouldValidate`
            table ("", TableDecl "T" NotRoot
              [ TableField 0 "x" (TVector Opt VString) False
              , TableField 1 "y" (TVector Opt VInt32) False
              , TableField 2 "z" (TVector Opt VBool) False
              ])

        it "where the elements are references (enum, struct, table, union)" $
          [r|
            namespace A;
            table Table { a: [B.E]; b: [B.EBF]; c: [B.S]; d: [B.T]; e: [B.U]; }

            namespace A.B;
            enum   E : int16 { EA }
            enum   EBF : uint16 (bit_flags) { EA }
            struct S { x: ubyte; y: int64; }
            table  T {}
            union  U { T }
          |] `shouldValidate` foldDecls
            [ table ("A", TableDecl "Table" NotRoot
                [ TableField 0 "a" (TVector Opt (VEnum   (TypeRef "A.B" "E") EInt16)) False
                , TableField 1 "b" (TVector Opt (VEnum   (TypeRef "A.B" "EBF") EWord16)) False
                , TableField 2 "c" (TVector Opt (VStruct (TypeRef "A.B" "S"))) False
                , TableField 3 "d" (TVector Opt (VTable  (TypeRef "A.B" "T"))) False
                , TableField 5 "e" (TVector Opt (VUnion  (TypeRef "A.B" "U"))) False
                ])
            , enum   ("A.B", EnumDecl "E" EInt16 False [EnumVal "EA" 0])
            , enum   ("A.B", EnumDecl "EBF" EWord16 True [EnumVal "EA" 1])
            , struct ("A.B", StructDecl "S" 8 16 [StructField "x" 7 0 SWord8, StructField "y" 0 8 SInt64])
            , table  ("A.B", TableDecl "T" NotRoot [])
            , union  ("A.B", UnionDecl "U" [UnionVal "T" (TypeRef "A.B" "T")])
            ]

        it "with `required` attribute" $
          [r| table T { x: [byte] (required); } |] `shouldValidate`
            table ("", TableDecl "T" NotRoot [ TableField 0 "x" (TVector Req VInt8) False ])

        it "with `deprecated` attribute" $
          [r| table T { x: [string] (deprecated); } |] `shouldValidate`
            table ("", TableDecl "T" NotRoot [ TableField 0 "x" (TVector Opt VString) True ])

        it "with default value" $ do
          let errorMsg = "[T.x]: default values currently only supported for scalar fields (integers, floating point, bool, enums)"
          [r| table T { x: [int] = a;   } |] `shouldFail` errorMsg
          [r| table T { x: [int] = 0;   } |] `shouldFail` errorMsg
          [r| table T { x: [int] = 0.0; } |] `shouldFail` errorMsg

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
            [ table ("", TableDecl "T" NotRoot
                [ TableField 0 "z" (TInt8 0) False
                , TableField 1 "x" (TInt8 0) False
                , TableField 2 "w" (TInt8 0) False
                , TableField 3 "y" (TInt8 0) False
                ]
              )
            ]

        it "id must be skipped when field is a union" $ do
          [r|
            union U { T }
            table T {
              x: U (id: 2);
              y: byte (id: 3);
              z: byte (id: 0);
            }
          |] `shouldValidate` foldDecls
            [ table ("", TableDecl "T" NotRoot
                [ TableField 0 "z" (TInt8 0) False
                , TableField 2 "x" (TUnion (TypeRef "" "U") Opt) False
                , TableField 3 "y" (TInt8 0) False
                ]
              )
            , union ("", UnionDecl "U" [UnionVal "T" (TypeRef "" "T")])
            ]
          [r|
            union U { T }
            table T {
              x: U (id: 1);
              y: byte (id: 2);
              z: byte (id: 0);
            }
          |] `shouldFail`
            "[T.x]: the id of a union field must be the last field's id + 2"

        it "id must be skipped when field is a vector of unions" $ do
          [r|
            union U { T }
            table T {
              x: [U] (id: 2);
              y: byte (id: 3);
              z: byte (id: 0);
            }
          |] `shouldValidate` foldDecls
            [ table ("", TableDecl "T" NotRoot
                [ TableField 0 "z" (TInt8 0) False
                , TableField 2 "x" (TVector Opt (VUnion (TypeRef "" "U"))) False
                , TableField 3 "y" (TInt8 0) False
                ]
              )
            , union ("", UnionDecl "U" [UnionVal "T" (TypeRef "" "T")])
            ]
          [r|
            union U { T }
            table T {
              x: [U] (id: 1);
              y: byte (id: 2);
              z: byte (id: 0);
            }
          |] `shouldFail`
            "[T.x]: the id of a vector of unions field must be the last field's id + 2"

        it "id can be a string, if it's coercible to an integer" $
          [r|
            table T {
              x: byte (id: "1");
              y: byte (id: "  02 ");
              z: byte (id: "0");
            }
          |] `shouldValidate` foldDecls
            [ table ("", TableDecl "T" NotRoot
                [ TableField 0 "z" (TInt8 0) False
                , TableField 1 "x" (TInt8 0) False
                , TableField 2 "y" (TInt8 0) False
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
            "[T.y]: field ids must be consecutive from 0; id 1 is missing"

        it "ids must start from 0" $
         [r| table T { x: byte (id: 2); y: int (id: 1); } |] `shouldFail`
            "[T.y]: field ids must be consecutive from 0; id 0 is missing"

        it "can't have duplicate ids" $
          [r| table T { x: byte (id: 0); y: int (id: 0); } |] `shouldFail`
            "[T.y]: field ids must be consecutive from 0; id 1 is missing"

    describe "unions" $ do
      it "simple" $
        [r|
          table T1{}
          table T2{}
          union U { T1, T2 }
        |] `shouldValidate` foldDecls
          [ table ("", TableDecl "T1" NotRoot [])
          , table ("", TableDecl "T2" NotRoot [])
          , union ("", UnionDecl "U"
              [ UnionVal "T1" (TypeRef "" "T1")
              , UnionVal "T2" (TypeRef "" "T2")
              ])
          ]

      it "with partially qualified type reference" $
        [r|
          namespace A.B;
          table T1{}
          table T2{}

          namespace A;
          union U { A.B.T1, B.T2 }
        |] `shouldValidate` foldDecls
          [ table ("A.B", TableDecl "T1" NotRoot [])
          , table ("A.B", TableDecl "T2" NotRoot [])
          , union ("A", UnionDecl "U"
              [ UnionVal "A_B_T1" (TypeRef "A.B" "T1")
              , UnionVal "B_T2"   (TypeRef "A.B" "T2")
              ])
          ]

      it "with alias" $
        [r|
          namespace A.B;
          table T1{}
          table T2{}

          namespace A;
          union U { Alias1 : A.B.T1, Alias2:B.T2 }
        |] `shouldValidate` foldDecls
          [ table ("A.B", TableDecl "T1" NotRoot [])
          , table ("A.B", TableDecl "T2" NotRoot [])
          , union ("A", UnionDecl "U"
              [ UnionVal "Alias1" (TypeRef "A.B" "T1")
              , UnionVal "Alias2" (TypeRef "A.B" "T2")
              ])
          ]

      it "union member must be a valid reference" $
        [r| union U { T } |] `shouldFail`
          "[U.T]: type 'T' does not exist (checked in these namespaces: '')"

      it "union members must be tables" $ do
        [r| union U { S }   struct S {x: byte;}      |] `shouldFail` "[U.S]: union members may only be tables"
        [r| union U { U2 }  union U2 {T}   table T{} |] `shouldFail` "[U.U2]: union members may only be tables"
        [r| union U { E }   enum E : int {X}         |] `shouldFail` "[U.E]: union members may only be tables"
        [r| union U { string }                       |] `shouldFail` "[U.string]: type 'string' does not exist (checked in these namespaces: '')"

      it "can't have duplicate identifiers" $ do
        [r| table T{}                union U {T, T}        |] `shouldFail` "[U]: 'T' declared more than once"
        [r| namespace A; table T{}   union U {A.T, A.T}    |] `shouldFail` "[A.U]: 'A_T' declared more than once"
        [r| namespace A; table T{}   union U {A.T, A_T: T} |] `shouldFail` "[A.U]: 'A_T' declared more than once"

      it "can't use NONE as an alias" $
        [r| table T{} union U {NONE: T} |] `shouldFail` "[U]: 'NONE' declared more than once"

      it "can't refer to a table named NONE" $
        [r| table NONE {} union U {NONE} |] `shouldFail` "[U]: 'NONE' declared more than once"

      it "can refer to a table named NONE, if using a qualified name" $
        [r|
          namespace A;
          table NONE {}
          union U {A.NONE}
        |] `shouldValidate` foldDecls
          [ table ("A", TableDecl "NONE" NotRoot [])
          , union ("A", UnionDecl "U"
              [ UnionVal "A_NONE"   (TypeRef "A" "NONE")
              ])
          ]

      it "can refer to a table named NONE, if using an alias" $
        [r|
          namespace A;
          table NONE {}
          union U {alias: NONE}
        |] `shouldValidate` foldDecls
          [ table ("A", TableDecl "NONE" NotRoot [])
          , union ("A", UnionDecl "U"
              [ UnionVal "alias"   (TypeRef "A" "NONE")
              ])
          ]

      it "can use the same table twice, if using a qualified name" $
        [r|
          namespace A;
          table T{}
          union U {T, A.T}
        |] `shouldValidate` foldDecls
          [ table ("A", TableDecl "T" NotRoot [])
          , union ("A", UnionDecl "U"
              [ UnionVal "T"   (TypeRef "A" "T")
              , UnionVal "A_T" (TypeRef "A" "T")
              ])
          ]

      it "can use the same table twice, if using an alias" $
        [r|
          namespace A;
          table T{}
          union U {T, alias:T}
        |] `shouldValidate` foldDecls
          [ table ("A", TableDecl "T" NotRoot [])
          , union ("A", UnionDecl "U"
              [ UnionVal "T"     (TypeRef "A" "T")
              , UnionVal "alias" (TypeRef "A" "T")
              ])
          ]






--           -- property: struct size (including paddings) = multiple of alignment
--           -- property: alignment max alignment ???

foldDecls :: [ValidDecls] -> ValidDecls
foldDecls = fold

enum :: (Namespace, EnumDecl) -> ValidDecls
enum e = SymbolTable [e] [] [] []

struct :: (Namespace, StructDecl) -> ValidDecls
struct s = SymbolTable [] [s] [] []

table :: (Namespace, TableDecl) -> ValidDecls
table t = SymbolTable [] [] [t] []

union :: (Namespace, UnionDecl) -> ValidDecls
union u = SymbolTable [] [] [] [u]

shouldSucceed :: HasCallStack => String -> Expectation
shouldSucceed input =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = FileTree "" schema []
      in  case validateSchemas schemas of
            Right _  -> pure ()
            Left err -> expectationFailure err

shouldValidate :: HasCallStack => String -> ValidDecls -> Expectation
shouldValidate input expectation =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = FileTree "" schema []
      in  validateSchemas schemas `shouldBe` Right (FileTree "" expectation [])

shouldFail :: HasCallStack => String -> String -> Expectation
shouldFail input expectedErrorMsg =
  case parse P.schema "" input of
    Left e -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = FileTree "" schema []
      in  validateSchemas schemas `shouldBe` Left expectedErrorMsg

showBundle :: (ShowErrorComponent e, Stream s) => ParseErrorBundle s e -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x = if null x
      then x
      else "  " ++ x
