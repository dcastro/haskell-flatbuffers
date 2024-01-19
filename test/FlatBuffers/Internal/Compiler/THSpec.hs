module FlatBuffers.Internal.Compiler.THSpec where

import Control.Arrow (second)

import Data.Bits ((.&.))
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import FlatBuffers.Internal.Build
import FlatBuffers.Internal.Compiler.Parser qualified as P
import FlatBuffers.Internal.Compiler.SemanticAnalysis (validateSchemas)
import FlatBuffers.Internal.Compiler.SyntaxTree (FileTree(..))
import FlatBuffers.Internal.Compiler.TH
import FlatBuffers.Internal.FileIdentifier (HasFileIdentifier(..), unsafeFileIdentifier)
import FlatBuffers.Internal.Read
import FlatBuffers.Internal.Types
import FlatBuffers.Internal.Write
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import TestImports
import Text.Megaparsec
  (ParseErrorBundle, ShowErrorComponent, TraversableStream, VisualStream, errorBundlePretty, parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  describe "TH" $ do
    describe "Tables" $ do
      it "with file identifier" $
        [r|
          table t {}
          root_type t;
          file_identifier "ABCD";
        |] `shouldCompileTo`
          [d|
            data T
            t :: WriteTable T
            t = writeTable []

            instance HasFileIdentifier T where
              getFileIdentifier = unsafeFileIdentifier (T.pack "ABCD")
          |]

      it "naming conventions" $ do
        let expected =
              [d|
                data SomePerson

                somePerson :: Maybe Int32 -> WriteTable SomePerson
                somePerson personAge = writeTable [ optionalDef 0 writeInt32TableField personAge ]

                somePersonPersonAge :: Table SomePerson -> Either ReadError Int32
                somePersonPersonAge = readTableFieldWithDef readInt32 0 0
              |]
        [r| table some_person  { person_age: int; }|] `shouldCompileTo` expected
        [r| table Some_Person  { Person_Age: int; }|] `shouldCompileTo` expected
        [r| table SomePerson   { PersonAge: int;  }|] `shouldCompileTo` expected
        [r| table somePerson   { personAge: int;  }|] `shouldCompileTo` expected

      it "with keyword as name" $
        [r| table Type {} |] `shouldCompileTo`
        [d|
          data Type
          type_ :: WriteTable Type
          type_ = writeTable []
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
                  [ optionalDef 0 writeWord8TableField    a
                  , optionalDef 0 writeWord16TableField   b
                  , optionalDef 0 writeWord32TableField   c
                  , optionalDef 0 writeWord64TableField   d
                  , optionalDef 0 writeInt8TableField     e
                  , optionalDef 0 writeInt16TableField    f
                  , optionalDef 0 writeInt32TableField    g
                  , optionalDef 0 writeInt64TableField    h
                  , optionalDef 0.0 writeFloatTableField  i
                  , optionalDef 0.0 writeDoubleTableField j
                  , optionalDef False writeBoolTableField k
                  ]

              scalarsA :: Table Scalars -> Either ReadError Word8
              scalarsA = readTableFieldWithDef readWord8   0 0
              scalarsB :: Table Scalars -> Either ReadError Word16
              scalarsB = readTableFieldWithDef readWord16  1 0
              scalarsC :: Table Scalars -> Either ReadError Word32
              scalarsC = readTableFieldWithDef readWord32  2 0
              scalarsD :: Table Scalars -> Either ReadError Word64
              scalarsD = readTableFieldWithDef readWord64  3 0
              scalarsE :: Table Scalars -> Either ReadError Int8
              scalarsE = readTableFieldWithDef readInt8    4 0
              scalarsF :: Table Scalars -> Either ReadError Int16
              scalarsF = readTableFieldWithDef readInt16   5 0
              scalarsG :: Table Scalars -> Either ReadError Int32
              scalarsG = readTableFieldWithDef readInt32   6 0
              scalarsH :: Table Scalars -> Either ReadError Int64
              scalarsH = readTableFieldWithDef readInt64   7 0
              scalarsI :: Table Scalars -> Either ReadError Float
              scalarsI = readTableFieldWithDef readFloat   8 0.0
              scalarsJ :: Table Scalars -> Either ReadError Double
              scalarsJ = readTableFieldWithDef readDouble  9 0.0
              scalarsK :: Table Scalars -> Either ReadError Bool
              scalarsK = readTableFieldWithDef readBool    10 False
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
                  [ optionalDef 8 writeWord8TableField          a
                  , optionalDef 16 writeWord16TableField        b
                  , optionalDef 32 writeWord32TableField        c
                  , optionalDef 64 writeWord64TableField        d
                  , optionalDef (-1) writeInt8TableField        e
                  , optionalDef (-2) writeInt16TableField       f
                  , optionalDef (-4) writeInt32TableField       g
                  , optionalDef (-8) writeInt64TableField       h
                  , optionalDef 3.9 writeFloatTableField        i
                  , optionalDef (-2.3e10) writeDoubleTableField j
                  , optionalDef True writeBoolTableField        k
                  ]

              scalarsA :: Table Scalars -> Either ReadError Word8
              scalarsA = readTableFieldWithDef readWord8   0 8
              scalarsB :: Table Scalars -> Either ReadError Word16
              scalarsB = readTableFieldWithDef readWord16  1 16
              scalarsC :: Table Scalars -> Either ReadError Word32
              scalarsC = readTableFieldWithDef readWord32  2 32
              scalarsD :: Table Scalars -> Either ReadError Word64
              scalarsD = readTableFieldWithDef readWord64  3 64
              scalarsE :: Table Scalars -> Either ReadError Int8
              scalarsE = readTableFieldWithDef readInt8    4 (-1)
              scalarsF :: Table Scalars -> Either ReadError Int16
              scalarsF = readTableFieldWithDef readInt16   5 -2
              scalarsG :: Table Scalars -> Either ReadError Int32
              scalarsG = readTableFieldWithDef readInt32   6 -4
              scalarsH :: Table Scalars -> Either ReadError Int64
              scalarsH = readTableFieldWithDef readInt64   7 -8
              scalarsI :: Table Scalars -> Either ReadError Float
              scalarsI = readTableFieldWithDef readFloat   8 3.9
              scalarsJ :: Table Scalars -> Either ReadError Double
              scalarsJ = readTableFieldWithDef readDouble  9 -2.3e10
              scalarsK :: Table Scalars -> Either ReadError Bool
              scalarsK = readTableFieldWithDef readBool    10 True
            |]

      describe "string fields" $ do
        it "normal field" $
          [r| table T {s: string;} |] `shouldCompileTo`
            [d|
              data T

              t :: Maybe Text -> WriteTable T
              t s = writeTable [optional writeTextTableField s]

              tS :: Table T -> Either ReadError (Maybe Text)
              tS = readTableFieldOpt readText 0
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
              t s = writeTable [writeTextTableField s]

              tS :: Table T -> Either ReadError Text
              tS = readTableFieldReq readText 0 "s"
            |]

      describe "enum fields" $
        it "are encoded as fields of the underlying type" $
          [r|
            enum Color:int8 { Red = 1, Blue }
            table T {x: Color = Blue; }
          |] `shouldCompileTo`
            [d|
              data Color = ColorRed | ColorBlue
                deriving (Eq, Show, Read, Ord, Bounded)

              toColor :: Int8 -> Maybe Color
              toColor n =
                case n of
                  1 -> Just ColorRed
                  2 -> Just ColorBlue
                  _ -> Nothing
              {-# INLINE toColor #-}

              fromColor :: Color -> Int8
              fromColor n =
                case n of
                  ColorRed  -> 1
                  ColorBlue -> 2
              {-# INLINE fromColor #-}

              colorName :: Color -> Text
              colorName c =
                case c of
                  ColorRed  -> T.pack "Red"
                  ColorBlue -> T.pack "Blue"
              {-# INLINE colorName #-}

              data T

              t :: Maybe Int8 -> WriteTable T
              t x = writeTable [ optionalDef 2 writeInt8TableField x ]

              tX :: Table T -> Either ReadError Int8
              tX = readTableFieldWithDef readInt8 0 2
            |]

      describe "enum fields with bit_flags" $
        it "are encoded as fields of the underlying type" $
          [r|
            enum Colors: ubyte (bit_flags) { Red = 2, Blue }
            table T {x: Colors = Blue; }
          |] `shouldCompileTo`
            [d|
              colorsRed :: Word8
              colorsRed = 4

              colorsBlue :: Word8
              colorsBlue = 8

              allColors :: [Word8]
              allColors = [ colorsRed, colorsBlue ]
              {-# INLINE allColors #-}

              colorsNames :: Word8 -> [Text]
              colorsNames c = res2
                where
                  res0 = []
                  res1 = if colorsBlue .&. c /= 0 then T.pack "Blue" : res0 else res0
                  res2 = if colorsRed  .&. c /= 0 then T.pack "Red"  : res1 else res1
              {-# INLINE colorsNames #-}

              data T

              t :: Maybe Word8 -> WriteTable T
              t x = writeTable [ optionalDef 8 writeWord8TableField x ]

              tX :: Table T -> Either ReadError Word8
              tX = readTableFieldWithDef readWord8 0 8
            |]

      describe "struct fields" $ do
        it "normal field" $
          [r|
            table T {x: S;}
            struct S {x: int;}
          |] `shouldCompileTo`
            [d|
              data S
              instance IsStruct S where
                structAlignmentOf = 4
                structSizeOf = 4

              s :: Int32 -> WriteStruct S
              s x = WriteStruct (buildInt32 x)

              sX :: Struct S -> Either ReadError Int32
              sX = readStructField readInt32 0

              data T
              t :: Maybe (WriteStruct S) -> WriteTable T
              t x = writeTable [optional writeStructTableField x]

              tX :: Table T -> Either ReadError (Maybe (Struct S))
              tX = readTableFieldOpt (Right . readStruct) 0
            |]

        it "deprecated" $
          [r|
            table T {x: S (deprecated);}
            struct S {x: int;}
          |] `shouldCompileTo`
            [d|
              data S
              instance IsStruct S where
                structAlignmentOf = 4
                structSizeOf = 4

              s :: Int32 -> WriteStruct S
              s x = WriteStruct (buildInt32 x)

              sX :: Struct S -> Either ReadError Int32
              sX = readStructField readInt32 0

              data T
              t ::  WriteTable T
              t = writeTable [deprecated]
            |]

        it "required" $
          [r|
            table T {X: S (required) ;}
            struct S {x: int;}
          |] `shouldCompileTo`
            [d|
              data S
              instance IsStruct S where
                structAlignmentOf = 4
                structSizeOf = 4

              s :: Int32 -> WriteStruct S
              s x = WriteStruct (buildInt32 x)

              sX :: Struct S -> Either ReadError Int32
              sX = readStructField readInt32 0

              data T
              t :: WriteStruct S -> WriteTable T
              t x = writeTable [writeStructTableField x]

              tX :: Table T -> Either ReadError (Struct S)
              tX = readTableFieldReq (Right . readStruct) 0 "X"
            |]

      describe "table fields" $ do
        it "normal field" $
          [r|
            table T1 {x: t2;}
            table t2{}
          |] `shouldCompileTo`
            [d|
              data T1
              t1 :: Maybe (WriteTable T2) -> WriteTable T1
              t1 x = writeTable [optional writeTableTableField x]

              t1X :: Table T1 -> Either ReadError (Maybe (Table T2))
              t1X = readTableFieldOpt readTable 0

              data T2
              t2 :: WriteTable T2
              t2 = writeTable []
            |]
        it "deprecated" $
          [r|
            table T1 {x: t2 (deprecated) ;}
            table t2{}
          |] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteTable T1
              t1 = writeTable [deprecated]

              data T2
              t2 :: WriteTable T2
              t2 = writeTable []
            |]
        it "required" $
          [r|
            table T1 {x: t2 (required) ;}
            table t2{}
          |] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteTable T2 -> WriteTable T1
              t1 x = writeTable [writeTableTableField x]

              t1X :: Table T1 -> Either ReadError (Table T2)
              t1X = readTableFieldReq readTable 0 "x"

              data T2
              t2 :: WriteTable T2
              t2 = writeTable []
            |]

      describe "union fields" $ do
        it "normal field" $
          [r|
            table t1 {x: u1;}
            union u1{t1}
          |] `shouldCompileTo`
            [d|
              data T1
              t1 :: Maybe (WriteUnion U1) -> WriteTable T1
              t1 x = writeTable
                [ optional writeUnionTypeTableField x
                , optional writeUnionValueTableField x
                ]

              t1X :: Table T1 -> Either ReadError (Union U1)
              t1X = readTableFieldUnion readU1 1

              data U1
                = U1T1 !(Table T1)

              u1T1 :: WriteTable T1 -> WriteUnion U1
              u1T1 = writeUnion 1

              readU1 :: Positive Word8 -> PositionInfo -> Either ReadError (Union U1)
              readU1 n pos =
                case getPositive n of
                  1  -> Union . U1T1 <$> readTable' pos
                  n' -> pure $! UnionUnknown n'
            |]

        it "deprecated" $
          [r|
            table t1 {x: u1 (deprecated) ;}
            union u1{t1}
          |] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteTable T1
              t1 = writeTable
                [ deprecated
                , deprecated
                ]

              data U1
                = U1T1 !(Table T1)

              u1T1 :: WriteTable T1 -> WriteUnion U1
              u1T1 = writeUnion 1

              readU1 :: Positive Word8 -> PositionInfo -> Either ReadError (Union U1)
              readU1 n pos =
                case getPositive n of
                  1  -> Union . U1T1 <$> readTable' pos
                  n' -> pure $! UnionUnknown n'
            |]

        it "required" $
          [r|
            table t1 {x: u1 (required) ;}
            union u1{t1}
          |] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteUnion U1 -> WriteTable T1
              t1 x = writeTable
                [ writeUnionTypeTableField x
                , writeUnionValueTableField x
                ]

              t1X :: Table T1 -> Either ReadError (Union U1)
              t1X = readTableFieldUnion readU1 1

              data U1
                = U1T1 !(Table T1)

              u1T1 :: WriteTable T1 -> WriteUnion U1
              u1T1 = writeUnion 1

              readU1 :: Positive Word8 -> PositionInfo -> Either ReadError (Union U1)
              readU1 n pos =
                case getPositive n of
                  1  -> Union . U1T1 <$> readTable' pos
                  n' -> pure $! UnionUnknown n'
            |]

      describe "vector fields" $ do
        it "deprecated" $
          [r|
            table t1 {
              a: [int8] (deprecated);
              b: [u1] (deprecated);
            }

            union u1{t1}
          |] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteTable T1
              t1 = writeTable [ deprecated, deprecated, deprecated ]

              data U1
                = U1T1 !(Table T1)

              u1T1 :: WriteTable T1 -> WriteUnion U1
              u1T1 = writeUnion 1

              readU1 :: Positive Word8 -> PositionInfo -> Either ReadError (Union U1)
              readU1 n pos =
                case getPositive n of
                  1  -> Union . U1T1 <$> readTable' pos
                  n' -> pure $! UnionUnknown n'
            |]
        describe "vector of numeric types / booolean" $ do
          it "normal" $
            [r|
              table t1 {
                a: [uint8];
                b: [uint16];
                c: [uint32];
                d: [uint64];
                e: [int8];
                f: [int16];
                g: [int32];
                h: [int64];
                i: [float32];
                j: [float64];
                k: [bool];
              }
            |] `shouldCompileTo`
              [d|
                data T1

                t1 ::
                     Maybe (WriteVector Word8)
                  -> Maybe (WriteVector Word16)
                  -> Maybe (WriteVector Word32)
                  -> Maybe (WriteVector Word64)
                  -> Maybe (WriteVector Int8)
                  -> Maybe (WriteVector Int16)
                  -> Maybe (WriteVector Int32)
                  -> Maybe (WriteVector Int64)
                  -> Maybe (WriteVector Float)
                  -> Maybe (WriteVector Double)
                  -> Maybe (WriteVector Bool)
                  -> WriteTable T1
                t1 a b c d e f g h i j k =
                  writeTable
                    [ optional writeVectorWord8TableField  a
                    , optional writeVectorWord16TableField b
                    , optional writeVectorWord32TableField c
                    , optional writeVectorWord64TableField d
                    , optional writeVectorInt8TableField   e
                    , optional writeVectorInt16TableField  f
                    , optional writeVectorInt32TableField  g
                    , optional writeVectorInt64TableField  h
                    , optional writeVectorFloatTableField  i
                    , optional writeVectorDoubleTableField j
                    , optional writeVectorBoolTableField   k
                    ]

                t1A :: Table T1 -> Either ReadError (Maybe (Vector Word8))
                t1A = readTableFieldOpt (readPrimVector VectorWord8)   0
                t1B :: Table T1 -> Either ReadError (Maybe (Vector Word16))
                t1B = readTableFieldOpt (readPrimVector VectorWord16)  1
                t1C :: Table T1 -> Either ReadError (Maybe (Vector Word32))
                t1C = readTableFieldOpt (readPrimVector VectorWord32)  2
                t1D :: Table T1 -> Either ReadError (Maybe (Vector Word64))
                t1D = readTableFieldOpt (readPrimVector VectorWord64)  3
                t1E :: Table T1 -> Either ReadError (Maybe (Vector Int8))
                t1E = readTableFieldOpt (readPrimVector VectorInt8)    4
                t1F :: Table T1 -> Either ReadError (Maybe (Vector Int16))
                t1F = readTableFieldOpt (readPrimVector VectorInt16)   5
                t1G :: Table T1 -> Either ReadError (Maybe (Vector Int32))
                t1G = readTableFieldOpt (readPrimVector VectorInt32)   6
                t1H :: Table T1 -> Either ReadError (Maybe (Vector Int64))
                t1H = readTableFieldOpt (readPrimVector VectorInt64)   7
                t1I :: Table T1 -> Either ReadError (Maybe (Vector Float))
                t1I = readTableFieldOpt (readPrimVector VectorFloat)   8
                t1J :: Table T1 -> Either ReadError (Maybe (Vector Double))
                t1J = readTableFieldOpt (readPrimVector VectorDouble)  9
                t1K :: Table T1 -> Either ReadError (Maybe (Vector Bool))
                t1K = readTableFieldOpt (readPrimVector VectorBool)    10
              |]

          it "required" $
            [r|
              table t1 {
                a: [uint8]   (required);
                b: [uint16]  (required);
                c: [uint32]  (required);
                d: [uint64]  (required);
                e: [int8]    (required);
                f: [int16]   (required);
                g: [int32]   (required);
                h: [int64]   (required);
                i: [float32] (required);
                j: [float64] (required);
                k: [bool]    (required);
              }
            |] `shouldCompileTo`
              [d|
                data T1

                t1 ::
                     WriteVector Word8
                  -> WriteVector Word16
                  -> WriteVector Word32
                  -> WriteVector Word64
                  -> WriteVector Int8
                  -> WriteVector Int16
                  -> WriteVector Int32
                  -> WriteVector Int64
                  -> WriteVector Float
                  -> WriteVector Double
                  -> WriteVector Bool
                  -> WriteTable T1
                t1 a b c d e f g h i j k =
                  writeTable
                    [ writeVectorWord8TableField  a
                    , writeVectorWord16TableField b
                    , writeVectorWord32TableField c
                    , writeVectorWord64TableField d
                    , writeVectorInt8TableField   e
                    , writeVectorInt16TableField  f
                    , writeVectorInt32TableField  g
                    , writeVectorInt64TableField  h
                    , writeVectorFloatTableField  i
                    , writeVectorDoubleTableField j
                    , writeVectorBoolTableField   k
                    ]

                t1A :: Table T1 -> Either ReadError (Vector Word8)
                t1A = readTableFieldReq (readPrimVector VectorWord8)   0 "a"
                t1B :: Table T1 -> Either ReadError (Vector Word16)
                t1B = readTableFieldReq (readPrimVector VectorWord16)  1 "b"
                t1C :: Table T1 -> Either ReadError (Vector Word32)
                t1C = readTableFieldReq (readPrimVector VectorWord32)  2 "c"
                t1D :: Table T1 -> Either ReadError (Vector Word64)
                t1D = readTableFieldReq (readPrimVector VectorWord64)  3 "d"
                t1E :: Table T1 -> Either ReadError (Vector Int8)
                t1E = readTableFieldReq (readPrimVector VectorInt8)    4 "e"
                t1F :: Table T1 -> Either ReadError (Vector Int16)
                t1F = readTableFieldReq (readPrimVector VectorInt16)   5 "f"
                t1G :: Table T1 -> Either ReadError (Vector Int32)
                t1G = readTableFieldReq (readPrimVector VectorInt32)   6 "g"
                t1H :: Table T1 -> Either ReadError (Vector Int64)
                t1H = readTableFieldReq (readPrimVector VectorInt64)   7 "h"
                t1I :: Table T1 -> Either ReadError (Vector Float)
                t1I = readTableFieldReq (readPrimVector VectorFloat)   8 "i"
                t1J :: Table T1 -> Either ReadError (Vector Double)
                t1J = readTableFieldReq (readPrimVector VectorDouble)  9 "j"
                t1K :: Table T1 -> Either ReadError (Vector Bool)
                t1K = readTableFieldReq (readPrimVector VectorBool)    10 "k"
              |]

        describe "vector of strings" $ do
          it "normal" $
            [r|
              table t1 { a: [string]; }
            |] `shouldCompileTo`
              [d|
                data T1
                t1 :: Maybe (WriteVector Text) -> WriteTable T1
                t1 a = writeTable [ optional writeVectorTextTableField a ]

                t1A :: Table T1 -> Either ReadError (Maybe (Vector Text))
                t1A = readTableFieldOpt (readPrimVector VectorText) 0
              |]
          it "required" $
            [r|
              table t1 { a: [string] (required); }
            |] `shouldCompileTo`
              [d|
                data T1
                t1 :: WriteVector Text -> WriteTable T1
                t1 a = writeTable [ writeVectorTextTableField a ]

                t1A :: Table T1 -> Either ReadError (Vector Text)
                t1A = readTableFieldReq (readPrimVector VectorText) 0 "a"
              |]

        describe "vector of enums" $ do
          it "normal" $
            [r|
              table t1 { a: [color]; }
              enum color : short { red }
            |] `shouldCompileTo`
              [d|
                data Color = ColorRed
                  deriving (Eq, Show, Read, Ord, Bounded)

                toColor :: Int16 -> Maybe Color
                toColor n =
                  case n of
                    0 -> Just ColorRed
                    _ -> Nothing
                {-# INLINE toColor #-}

                fromColor :: Color -> Int16
                fromColor n = case n of ColorRed -> 0
                {-# INLINE fromColor #-}

                colorName :: Color -> Text
                colorName c = case c of ColorRed -> T.pack "red"
                {-# INLINE colorName #-}

                data T1
                t1 :: Maybe (WriteVector Int16) -> WriteTable T1
                t1 a = writeTable
                  [ optional writeVectorInt16TableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Maybe (Vector Int16))
                t1A = readTableFieldOpt (readPrimVector VectorInt16) 0
              |]

          it "required" $
            [r|
              table t1 { a: [color] (required); }
              enum color : short { red }
            |] `shouldCompileTo`
              [d|
                data Color = ColorRed
                  deriving (Eq, Show, Read, Ord, Bounded)

                toColor :: Int16 -> Maybe Color
                toColor n =
                  case n of
                    0 -> Just ColorRed
                    _ -> Nothing
                {-# INLINE toColor #-}

                fromColor :: Color -> Int16
                fromColor n = case n of ColorRed -> 0
                {-# INLINE fromColor #-}

                colorName :: Color -> Text
                colorName c = case c of ColorRed -> T.pack "red"
                {-# INLINE colorName #-}

                data T1
                t1 :: WriteVector Int16 -> WriteTable T1
                t1 a = writeTable
                  [ writeVectorInt16TableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Vector Int16)
                t1A = readTableFieldReq (readPrimVector VectorInt16) 0 "a"
              |]

        describe "vector of enums with bit_flags" $ do
          it "normal" $
            [r|
              table t1 { a: [colors]; }
              enum colors : ulong (bit_flags) { red = 20 }
            |] `shouldCompileTo`
              [d|
                colorsRed :: Word64
                colorsRed = 1048576

                allColors :: [Word64]
                allColors = [ colorsRed ]
                {-# INLINE allColors #-}

                colorsNames :: Word64 -> [Text]
                colorsNames c = res1
                  where
                    res0 = []
                    res1 = if colorsRed .&. c /= 0 then T.pack "red" : res0 else res0
                {-# INLINE colorsNames #-}

                data T1
                t1 :: Maybe (WriteVector Word64) -> WriteTable T1
                t1 a = writeTable
                  [ optional writeVectorWord64TableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Maybe (Vector Word64))
                t1A = readTableFieldOpt (readPrimVector VectorWord64) 0
              |]

          it "required" $
            [r|
              table t1 { a: [colors] (required); }
              enum colors : uint64 (bit_flags) { red = 63 }
            |] `shouldCompileTo`
              [d|
                colorsRed :: Word64
                colorsRed = 9223372036854775808

                allColors :: [Word64]
                allColors = [ colorsRed ]
                {-# INLINE allColors #-}

                colorsNames :: Word64 -> [Text]
                colorsNames c = res1
                  where
                    res0 = []
                    res1 = if colorsRed .&. c /= 0 then T.pack "red" : res0 else res0
                {-# INLINE colorsNames #-}

                data T1
                t1 :: WriteVector Word64 -> WriteTable T1
                t1 a = writeTable
                  [ writeVectorWord64TableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Vector Word64)
                t1A = readTableFieldReq (readPrimVector VectorWord64) 0 "a"
              |]

        describe "vector of structs" $ do
          it "normal" $
            [r|
              table t1 { a: [s1]; }
              struct s1 (force_align: 8) { a: ubyte; }
            |] `shouldCompileTo`
              [d|
                data S1
                instance IsStruct S1 where
                  structAlignmentOf = 8
                  structSizeOf = 8

                s1 :: Word8 -> WriteStruct S1
                s1 a = WriteStruct (buildWord8 a <> buildPadding 7)

                s1A :: Struct S1 -> Either ReadError Word8
                s1A = readStructField readWord8 0

                data T1
                t1 :: Maybe (WriteVector (WriteStruct S1)) -> WriteTable T1
                t1 a = writeTable
                  [ optional writeVectorStructTableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Maybe (Vector (Struct S1)))
                t1A = readTableFieldOpt (readPrimVector VectorStruct) 0
              |]

          it "required" $
            [r|
              table t1 { a: [s1] (required); }
              struct s1 (force_align: 8) { a: ubyte; }
            |] `shouldCompileTo`
              [d|
                data S1
                instance IsStruct S1 where
                  structAlignmentOf = 8
                  structSizeOf = 8

                s1 :: Word8 -> WriteStruct S1
                s1 a = WriteStruct (buildWord8 a <> buildPadding 7)

                s1A :: Struct S1 -> Either ReadError Word8
                s1A = readStructField readWord8 0

                data T1
                t1 :: WriteVector (WriteStruct S1) -> WriteTable T1
                t1 a = writeTable
                  [ writeVectorStructTableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Vector (Struct S1))
                t1A = readTableFieldReq (readPrimVector VectorStruct) 0 "a"
              |]

        describe "vector of tables" $ do
          it "normal" $
            [r|
              table t1 { a: [t1]; }
            |] `shouldCompileTo`
              [d|
                data T1
                t1 :: Maybe (WriteVector (WriteTable T1)) -> WriteTable T1
                t1 a = writeTable
                  [ optional writeVectorTableTableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Maybe (Vector (Table T1)))
                t1A = readTableFieldOpt readTableVector 0
              |]
          it "required" $
            [r|
              table t1 { a: [t1] (required); }
            |] `shouldCompileTo`
              [d|
                data T1
                t1 :: WriteVector (WriteTable T1) -> WriteTable T1
                t1 a = writeTable
                  [ writeVectorTableTableField a
                  ]

                t1A :: Table T1 -> Either ReadError (Vector (Table T1))
                t1A = readTableFieldReq readTableVector 0 "a"
              |]

        describe "vector of unions" $ do
          it "normal" $
            [r|
              table t1 {x: [u1];}
              union u1{t1}
            |] `shouldCompileTo`
              [d|
                data T1
                t1 :: Maybe (WriteVector (WriteUnion U1)) -> WriteTable T1
                t1 x = writeTable
                  [ optional writeUnionTypesVectorTableField x
                  , optional writeUnionValuesVectorTableField x
                  ]

                t1X :: Table T1 -> Either ReadError (Maybe (Vector (Union U1)))
                t1X = readTableFieldUnionVectorOpt readU1 1

                data U1
                  = U1T1 !(Table T1)

                u1T1 :: WriteTable T1 -> WriteUnion U1
                u1T1 = writeUnion 1

                readU1 :: Positive Word8 -> PositionInfo -> Either ReadError (Union U1)
                readU1 n pos =
                  case getPositive n of
                    1  -> Union . U1T1 <$> readTable' pos
                    n' -> pure $! UnionUnknown n'
              |]

          it "required" $
            [r|
              table t1 {x: [u1] (required);}
              union u1{t1}
            |] `shouldCompileTo`
              [d|
                data T1
                t1 :: WriteVector (WriteUnion U1) -> WriteTable T1
                t1 x = writeTable
                  [ writeUnionTypesVectorTableField x
                  , writeUnionValuesVectorTableField x
                  ]

                t1X :: Table T1 -> Either ReadError (Vector (Union U1))
                t1X = readTableFieldUnionVectorReq readU1 1 "x"

                data U1
                  = U1T1 !(Table T1)

                u1T1 :: WriteTable T1 -> WriteUnion U1
                u1T1 = writeUnion 1

                readU1 :: Positive Word8 -> PositionInfo -> Either ReadError (Union U1)
                readU1 n pos =
                  case getPositive n of
                    1  -> Union . U1T1 <$> readTable' pos
                    n' -> pure $! UnionUnknown n'
              |]

    describe "Enums" $
      it "naming conventions" $ do
        let expected redName greenName =
              [d|
                data MyColor = MyColorIsRed | MyColorIsGreen
                  deriving (Eq, Show, Read, Ord, Bounded)

                toMyColor :: Int16 -> Maybe MyColor
                toMyColor n =
                  case n of
                    -2 -> Just MyColorIsRed
                    -1 -> Just MyColorIsGreen
                    _  -> Nothing
                {-# INLINE toMyColor #-}

                fromMyColor :: MyColor -> Int16
                fromMyColor n =
                  case n of
                    MyColorIsRed   -> -2
                    MyColorIsGreen -> -1
                {-# INLINE fromMyColor #-}

                myColorName :: MyColor -> Text
                myColorName c =
                  case c of
                    MyColorIsRed   -> T.pack $(stringE redName)
                    MyColorIsGreen -> T.pack $(stringE greenName)
                {-# INLINE myColorName #-}
              |]

        [r| enum my_color: int16 { is_red = -2, is_green  } |] `shouldCompileTo` expected "is_red" "is_green"
        [r| enum My_Color: int16 { Is_Red = -2, Is_Green  } |] `shouldCompileTo` expected "Is_Red" "Is_Green"
        [r| enum MyColor:  int16 { IsRed = -2,  IsGreen   } |] `shouldCompileTo` expected "IsRed"  "IsGreen"
        [r| enum myColor:  int16 { isRed = -2,  isGreen   } |] `shouldCompileTo` expected "isRed"  "isGreen"


    describe "Enums with bit_flags" $
      it "naming conventions" $ do
        let expected redName greenName =
              [d|
                myColorsIsRed :: Word16
                myColorsIsRed = 4
                myColorsIsGreen :: Word16
                myColorsIsGreen = 8

                allMyColors :: [Word16]
                allMyColors = [ myColorsIsRed, myColorsIsGreen ]
                {-# INLINE allMyColors #-}

                myColorsNames :: Word16 -> [Text]
                myColorsNames c = res2
                  where
                    res0 = []
                    res1 = if myColorsIsGreen .&. c /= 0 then T.pack $(stringE greenName) : res0 else res0
                    res2 = if myColorsIsRed   .&. c /= 0 then T.pack $(stringE redName)   : res1 else res1
                {-# INLINE myColorsNames #-}
              |]

        [r| enum my_colors: ushort (bit_flags) { is_red = 2, is_green  } |] `shouldCompileTo` expected "is_red" "is_green"
        [r| enum My_Colors: ushort (bit_flags) { Is_Red = 2, Is_Green  } |] `shouldCompileTo` expected "Is_Red" "Is_Green"
        [r| enum MyColors:  ushort (bit_flags) { IsRed = 2,  IsGreen   } |] `shouldCompileTo` expected "IsRed"  "IsGreen"
        [r| enum myColors:  ushort (bit_flags) { isRed = 2,  isGreen   } |] `shouldCompileTo` expected "isRed"  "isGreen"

    describe "Structs" $ do
      it "naming conventions" $ do
        let expected =
              [d|
                data MyStruct
                instance IsStruct MyStruct where
                  structAlignmentOf = 4
                  structSizeOf = 4

                myStruct :: Int32 -> WriteStruct MyStruct
                myStruct myField = WriteStruct (buildInt32 myField)

                myStructMyField :: Struct MyStruct -> Either ReadError Int32
                myStructMyField = readStructField readInt32 0
              |]
        [r| struct my_struct { my_field: int; } |] `shouldCompileTo` expected
        [r| struct My_Struct { My_Field: int; } |] `shouldCompileTo` expected
        [r| struct MyStruct  { MyField: int;  } |] `shouldCompileTo` expected
        [r| struct myStruct  { myField: int;  } |] `shouldCompileTo` expected

      it "with primitive fields" $
        [r|
          struct Scalars {
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
            instance IsStruct Scalars where
              structAlignmentOf = 8
              structSizeOf = 56

            scalars ::
                 Word8
              -> Word16
              -> Word32
              -> Word64
              -> Int8
              -> Int16
              -> Int32
              -> Int64
              -> Float
              -> Double
              -> Bool
              -> WriteStruct Scalars
            scalars a b c d e f g h i j k =
              WriteStruct (
                buildWord8 a <> buildPadding 1 <> buildWord16 b <> buildWord32 c
                <> buildWord64 d
                <> buildInt8 e <> buildPadding 1 <> buildInt16 f <> buildInt32 g
                <> buildInt64 h
                <> buildFloat i <> buildPadding 4
                <> buildDouble j
                <> buildBool k <> buildPadding 7
              )

            scalarsA :: Struct Scalars -> Either ReadError Word8
            scalarsA = readStructField readWord8 0
            scalarsB :: Struct Scalars -> Either ReadError Word16
            scalarsB = readStructField readWord16 2
            scalarsC :: Struct Scalars -> Either ReadError Word32
            scalarsC = readStructField readWord32 4
            scalarsD :: Struct Scalars -> Either ReadError Word64
            scalarsD = readStructField readWord64 8
            scalarsE :: Struct Scalars -> Either ReadError Int8
            scalarsE = readStructField readInt8 16
            scalarsF :: Struct Scalars -> Either ReadError Int16
            scalarsF = readStructField readInt16 18
            scalarsG :: Struct Scalars -> Either ReadError Int32
            scalarsG = readStructField readInt32 20
            scalarsH :: Struct Scalars -> Either ReadError Int64
            scalarsH = readStructField readInt64 24
            scalarsI :: Struct Scalars -> Either ReadError Float
            scalarsI = readStructField readFloat 32
            scalarsJ :: Struct Scalars -> Either ReadError Double
            scalarsJ = readStructField readDouble 40
            scalarsK :: Struct Scalars -> Either ReadError Bool
            scalarsK = readStructField readBool 48
          |]

      it "with enum fields" $
        [r|
          struct S { e: E; }
          enum E : byte { X }
        |] `shouldCompileTo`
          [d|
            data E = EX
              deriving (Eq, Show, Read, Ord, Bounded)

            toE :: Int8 -> Maybe E
            toE n = case n of
              0 -> Just EX
              _ -> Nothing
            {-# INLINE toE #-}

            fromE :: E -> Int8
            fromE n = case n of EX -> 0
            {-# INLINE fromE #-}

            eName :: E -> Text
            eName c = case c of EX -> T.pack "X"
            {-# INLINE eName #-}

            data S
            instance IsStruct S where
              structAlignmentOf = 1
              structSizeOf = 1

            s :: Int8 -> WriteStruct S
            s e = WriteStruct (buildInt8 e)

            sE :: Struct S -> Either ReadError Int8
            sE = readStructField readInt8 0
          |]

      it "with enum fields with bit_flags" $
        [r|
          struct S { e: E; }
          enum E : ubyte (bit_flags) { X }
        |] `shouldCompileTo`
          [d|
            eX :: Word8
            eX = 1

            allE :: [Word8]
            allE = [ eX ]
            {-# INLINE allE #-}

            eNames :: Word8 -> [Text]
            eNames c = res1
              where
                res0 = []
                res1 = if eX .&. c /= 0 then T.pack "X" : res0 else res0
            {-# INLINE eNames #-}

            data S
            instance IsStruct S where
              structAlignmentOf = 1
              structSizeOf = 1

            s :: Word8 -> WriteStruct S
            s e = WriteStruct (buildWord8 e)

            sE :: Struct S -> Either ReadError Word8
            sE = readStructField readWord8 0
          |]

      it "with nested structs" $
        [r|
          struct S1 (force_align: 2) { s2: S2; }
          struct S2 { x: int8; }
        |] `shouldCompileTo`
          [d|
            data S1
            instance IsStruct S1 where
              structAlignmentOf = 2
              structSizeOf = 2

            s1 :: WriteStruct S2 -> WriteStruct S1
            s1 s2 = WriteStruct (buildStruct s2 <> buildPadding 1)

            s1S2 :: Struct S1 -> Struct S2
            s1S2 = readStructField readStruct 0

            data S2
            instance IsStruct S2 where
              structAlignmentOf = 1
              structSizeOf = 1

            s2 :: Int8 -> WriteStruct S2
            s2 x = WriteStruct (buildInt8 x)

            s2X :: Struct S2 -> Either ReadError Int8
            s2X = readStructField readInt8 0
          |]

      it "with keyword as name" $
        [r| struct Type { x : byte; } |] `shouldCompileTo`
        [d|
          data Type
          instance IsStruct Type
              where structAlignmentOf = 1
                    structSizeOf = 1

          type_ :: Int8 -> WriteStruct Type
          type_ x = WriteStruct (buildInt8 x)

          typeX :: Struct Type -> Either ReadError Int8
          typeX = readStructField readInt8 0
        |]

    describe "Unions" $
      it "naming conventions" $ do
        let expected =
              [d|
                data MySword
                mySword :: WriteTable MySword
                mySword = writeTable []

                data MyWeapon
                  = MyWeaponMySword !(Table MySword)
                  | MyWeaponMyAlias !(Table MySword)

                myWeaponMySword :: WriteTable MySword -> WriteUnion MyWeapon
                myWeaponMySword = writeUnion 1

                myWeaponMyAlias :: WriteTable MySword -> WriteUnion MyWeapon
                myWeaponMyAlias = writeUnion 2

                readMyWeapon :: Positive Word8 -> PositionInfo -> Either ReadError (Union MyWeapon)
                readMyWeapon n pos =
                  case getPositive n of
                    1  -> Union . MyWeaponMySword <$> readTable' pos
                    2  -> Union . MyWeaponMyAlias <$> readTable' pos
                    n' -> pure $! UnionUnknown n'
              |]

        [r| table my_sword{} union my_weapon { my_sword, my_alias: my_sword } |] `shouldCompileTo` expected
        [r| table My_sword{} union My_weapon { My_sword, My_alias: My_sword } |] `shouldCompileTo` expected
        [r| table MySword{}  union MyWeapon  { MySword,  MyAlias:  MySword  } |] `shouldCompileTo` expected
        [r| table mySword{}  union myWeapon  { mySword,  myAlias:  mySword  } |] `shouldCompileTo` expected



shouldCompileTo :: HasCallStack => String -> Q [Dec] -> Expectation
shouldCompileTo input expectedQ =
  case parse P.schema "" input of
    Left e       -> expectationFailure $ "Parsing failed with error:\n" <> showBundle e
    Right schema ->
      let schemas = FileTree "" schema mempty
      in  case validateSchemas schemas of
        Left err                  -> expectationFailure err
        Right (FileTree _ root _) -> do
          ast <- runQ (compileSymbolTable root)
          expected <- runQ expectedQ
          PrettyAst (normalizeDec <$> ast) `shouldBe` PrettyAst (normalizeDec <$> expected)

newtype PrettyAst = PrettyAst [Dec]
  deriving Eq

instance Show PrettyAst where
  show (PrettyAst decs) = pprint decs

showBundle :: (ShowErrorComponent e, TraversableStream s, VisualStream s) => ParseErrorBundle s e -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x = if null x
      then x
      else "  " ++ x

-- | This function normalize ASTs to make them comparable.
--   * ASTs obtained from quasiquotes (like what we're doing in these tests) use `newName`, whereas we often use `mkName`.
--     So we have to normalize names here.
--   * Declarations like `x = 5` are interpreted as a value declaration, but they're equivalent to a
--     function declaration with a single clause and a single pattern.
normalizeDec :: Dec -> Dec
normalizeDec dec = valToFun $
  case dec of
    DataD a name b c cons e -> DataD a (normalizeName name) b c (normalizeCon <$> cons) e
    SigD n t -> SigD (normalizeName n) (normalizeType t)
    FunD n clauses -> FunD (normalizeName n) (normalizeClause <$> clauses)
    ValD pat body decs -> ValD (normalizePat pat) (normalizeBody body) (normalizeDec <$> decs)
    PragmaD p -> PragmaD (normalizePragma p)
    ClassD cxt n tvs funDeps decs ->
      ClassD
        (normalizeType <$> cxt)
        (normalizeName n)
        (normalizeTyVarBndr <$> tvs)
        funDeps
        (normalizeDec <$> decs)

    InstanceD overlap cxt typ decs ->
      InstanceD
        overlap
        (normalizeType <$> cxt)
        (normalizeType typ)
        (normalizeDec <$> decs)
    _ -> dec

-- | values with a simple variable pattern (e.g. `x = 5`) are equivalent to functions with only one clause and no parameters
valToFun :: Dec -> Dec
valToFun dec =
  case dec of
    ValD (VarP name) body decs -> FunD name [Clause [] body decs]
    _                          -> dec

normalizePragma :: Pragma -> Pragma
normalizePragma p =
  case p of
    InlineP n i rm p -> InlineP (normalizeName n) i rm p
    _                -> p

normalizeCon :: Con -> Con
normalizeCon c =
  case c of
    NormalC name bangTypes -> NormalC (normalizeName name) (second normalizeType <$> bangTypes)
    _ -> c

normalizeType :: Type -> Type
normalizeType t =
  case t of
    ConT n -> ConT (normalizeName n)
    VarT n -> VarT (normalizeName n)
    AppT t1 t2 -> AppT (normalizeType t1) (normalizeType t2)
    ForallT tvs cxt t ->  ForallT (normalizeTyVarBndr <$> tvs) (normalizeType <$> cxt) (normalizeType t)
    _ -> t

normalizeTyVarBndr :: TyVarBndr flag -> TyVarBndr flag
normalizeTyVarBndr tv =
  case tv of
    PlainTV n flag    -> PlainTV (normalizeName n) flag
    KindedTV n flag k -> KindedTV (normalizeName n) flag (normalizeType k)

normalizeClause :: Clause -> Clause
normalizeClause (Clause pats body decs) = Clause (normalizePat <$> pats) (normalizeBody body) (normalizeDec <$> decs)

normalizePat :: Pat -> Pat
normalizePat p =
  case p of
    VarP n      -> VarP (normalizeName n)
    ConP n types pats -> ConP (normalizeName n) (normalizeType <$> types) (normalizePat <$> pats)
    TupP pats   -> TupP (normalizePat <$> pats)
    _           -> p

normalizeBody :: Body -> Body
normalizeBody b =
  case b of
    NormalB e -> NormalB (normalizeExp e)
    _         -> b

normalizeExp :: Exp -> Exp
normalizeExp e =
  case e of
    VarE n -> VarE (normalizeName n)
    AppE e1 e2 -> AppE (normalizeExp e1) (normalizeExp e2)
    ListE es -> ListE (normalizeExp <$> es)
    CaseE e matches -> CaseE (normalizeExp e) (normalizeMatch <$> matches)
    ConE name
      | name == '[] -> ListE []
      | otherwise -> ConE (normalizeName name)
    InfixE l op r -> InfixE (normalizeExp <$> l) (normalizeExp op) (normalizeExp <$> r)
    CondE b t f -> CondE (normalizeExp b) (normalizeExp t) (normalizeExp f)
    _ -> e

normalizeMatch :: Match -> Match
normalizeMatch (Match pat body decs) =
  Match (normalizePat pat) (normalizeBody body) (normalizeDec <$> decs)

normalizeName :: Name -> Name
normalizeName (Name (OccName occ) (NameU _)) = mkName occ
normalizeName name                           = name
