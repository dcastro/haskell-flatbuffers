{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ExplicitForAll #-}

module FlatBuffers.Internal.Compiler.THSpec where

import           Control.Arrow                                  ( second )

import           Data.Int
import           Data.Text                                      ( Text )
import qualified Data.Text                                      as T
import           Data.Word

import qualified FlatBuffers.Internal.Compiler.Parser           as P
import           FlatBuffers.Internal.Compiler.SemanticAnalysis ( SymbolTable(..), validateSchemas )
import           FlatBuffers.Internal.Compiler.SyntaxTree       ( FileTree(..), HasIdent(..), Ident(..), Namespace(..) )
import           FlatBuffers.Internal.Compiler.TH
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree
import           FlatBuffers.Internal.Positive                  ( Positive(getPositive) )
import           FlatBuffers.Read
import           FlatBuffers.Write

import           Language.Haskell.TH
import           Language.Haskell.TH.Cleanup                    ( simplifiedTH )
import           Language.Haskell.TH.Syntax

import           System.IO.Unsafe                               ( unsafePerformIO )

import           Test.Hspec

import           TestUtils

import           Text.Megaparsec                                ( ParseErrorBundle, ShowErrorComponent, Stream, errorBundlePretty, parse )
import           Text.RawString.QQ                              ( r )


spec :: Spec
spec =
  describe "TH" $ do
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

        it "table field names are lowercased in params, camelCased in getters" $
          [r| table T { X: int; }|] `shouldCompileTo`
            [d|
              data T

              t :: Maybe Int32 -> WriteTable T
              t x = writeTable [ optionalDef 0 (inline int32) x ]

              tX :: forall m. ReadCtx m => Table T -> m Int32
              tX = readTableFieldWithDef readInt32 0 0
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

              scalarsA :: forall m. ReadCtx m => Table Scalars -> m Word8
              scalarsA = readTableFieldWithDef readWord8   0 0
              scalarsB :: forall m. ReadCtx m => Table Scalars -> m Word16
              scalarsB = readTableFieldWithDef readWord16  1 0
              scalarsC :: forall m. ReadCtx m => Table Scalars -> m Word32
              scalarsC = readTableFieldWithDef readWord32  2 0
              scalarsD :: forall m. ReadCtx m => Table Scalars -> m Word64
              scalarsD = readTableFieldWithDef readWord64  3 0
              scalarsE :: forall m. ReadCtx m => Table Scalars -> m Int8
              scalarsE = readTableFieldWithDef readInt8    4 0
              scalarsF :: forall m. ReadCtx m => Table Scalars -> m Int16
              scalarsF = readTableFieldWithDef readInt16   5 0
              scalarsG :: forall m. ReadCtx m => Table Scalars -> m Int32
              scalarsG = readTableFieldWithDef readInt32   6 0
              scalarsH :: forall m. ReadCtx m => Table Scalars -> m Int64
              scalarsH = readTableFieldWithDef readInt64   7 0
              scalarsI :: forall m. ReadCtx m => Table Scalars -> m Float
              scalarsI = readTableFieldWithDef readFloat   8 0.0
              scalarsJ :: forall m. ReadCtx m => Table Scalars -> m Double
              scalarsJ = readTableFieldWithDef readDouble  9 0.0
              scalarsK :: forall m. ReadCtx m => Table Scalars -> m Bool
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

              scalarsA :: forall m. ReadCtx m => Table Scalars -> m Word8
              scalarsA = readTableFieldWithDef readWord8   0 8
              scalarsB :: forall m. ReadCtx m => Table Scalars -> m Word16
              scalarsB = readTableFieldWithDef readWord16  1 16
              scalarsC :: forall m. ReadCtx m => Table Scalars -> m Word32
              scalarsC = readTableFieldWithDef readWord32  2 32
              scalarsD :: forall m. ReadCtx m => Table Scalars -> m Word64
              scalarsD = readTableFieldWithDef readWord64  3 64
              scalarsE :: forall m. ReadCtx m => Table Scalars -> m Int8
              scalarsE = readTableFieldWithDef readInt8    4 (-1)
              scalarsF :: forall m. ReadCtx m => Table Scalars -> m Int16
              scalarsF = readTableFieldWithDef readInt16   5 -2
              scalarsG :: forall m. ReadCtx m => Table Scalars -> m Int32
              scalarsG = readTableFieldWithDef readInt32   6 -4
              scalarsH :: forall m. ReadCtx m => Table Scalars -> m Int64
              scalarsH = readTableFieldWithDef readInt64   7 -8
              scalarsI :: forall m. ReadCtx m => Table Scalars -> m Float
              scalarsI = readTableFieldWithDef readFloat   8 3.9
              scalarsJ :: forall m. ReadCtx m => Table Scalars -> m Double
              scalarsJ = readTableFieldWithDef readDouble  9 -2.3e10
              scalarsK :: forall m. ReadCtx m => Table Scalars -> m Bool
              scalarsK = readTableFieldWithDef readBool    10 True
            |]

      describe "string fields" $ do
        it "normal field" $
          [r| table T {s: string;} |] `shouldCompileTo`
            [d|
              data T

              t :: Maybe Text -> WriteTable T
              t s = writeTable [optional text s]

              tS :: forall m. ReadCtx m => Table T -> m (Maybe Text)
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
              t s = writeTable [text s]

              tS :: forall m. ReadCtx m => Table T -> m Text
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

              fromColor :: Color -> Int8
              fromColor n =
                case n of
                  ColorRed -> 1
                  ColorBlue -> 2

              data T

              t :: Maybe Int8 -> WriteTable T
              t x = writeTable [ optionalDef 2 (inline int8) x ]

              tX :: forall m. ReadCtx m => Table T -> m Int8
              tX = readTableFieldWithDef readInt8 0 2
            |]

      describe "struct fields" $ do
        it "normal field" $
          [r| table T {x: S;} struct S {x: int;}|] `shouldCompileTo`
            [d|
              data S
              s :: Int32 -> WriteStruct S
              s x = writeStruct 4 [ int32 x ]

              sX :: forall m. ReadCtx m => Struct S -> m Int32
              sX = readStructField readInt32 0

              data T
              t :: Maybe (WriteStruct S) -> WriteTable T
              t x = writeTable [optional (inline unWriteStruct) x]

              tX :: forall m. ReadCtx m => Table T -> m (Maybe (Struct S))
              tX = readTableFieldOpt readStruct' 0
            |]

        it "deprecated" $
          [r| table T {x: S (deprecated);} struct S {x: int;}|] `shouldCompileTo`
            [d|
              data S
              s :: Int32 -> WriteStruct S
              s x = writeStruct 4 [ int32 x ]

              sX :: forall m. ReadCtx m => Struct S -> m Int32
              sX = readStructField readInt32 0

              data T
              t ::  WriteTable T
              t = writeTable [deprecated]
            |]

        it "required" $
          [r| table T {X: S (required) ;} struct S {x: int;}|] `shouldCompileTo`
            [d|
              data S
              s :: Int32 -> WriteStruct S
              s x = writeStruct 4 [ int32 x ]

              sX :: forall m. ReadCtx m => Struct S -> m Int32
              sX = readStructField readInt32 0

              data T
              t :: WriteStruct S -> WriteTable T
              t x = writeTable [inline unWriteStruct x]

              tX :: forall m. ReadCtx m => Table T -> m (Struct S)
              tX = readTableFieldReq readStruct' 0 "X"
            |]

      describe "table fields" $ do
        it "normal field" $
          [r| table T1 {x: t2;} table t2{}|] `shouldCompileTo`
            [d|
              data T1
              t1 :: Maybe (WriteTable T2) -> WriteTable T1
              t1 x = writeTable [optional unWriteTable x]

              t1X :: forall m. ReadCtx m => Table T1 -> m (Maybe (Table T2))
              t1X = readTableFieldOpt readTable 0

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

              t1X :: forall m. ReadCtx m => Table T1 -> m (Table T2)
              t1X = readTableFieldReq readTable 0 "x"

              data T2
              t2 :: WriteTable T2
              t2 = writeTable []
            |]

      describe "union fields" $ do
        it "normal field" $
          [r| table t1 {x: u1;} union u1{t1}|] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteUnion U1 -> WriteTable T1
              t1 x = writeTable
                [ writeUnionType x
                , writeUnionValue x
                ]

              t1X :: forall m. ReadCtx m => Table T1 -> m (Union U1)
              t1X = readTableFieldUnion readU1 1

              data U1
                = U1T1 !(Table T1)

              class WriteU1 a where
                u1 :: WriteTable a -> WriteUnion U1

              instance WriteU1 T1 where
                u1 = writeUnion 1

              readU1 :: forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union U1)
              readU1 n pos =
                case getPositive n of
                  1  -> Union . U1T1 <$> readTable pos
                  n' -> pure $! UnionUnknown n'
            |]

        it "deprecated" $
          [r| table t1 {x: u1 (deprecated) ;} union u1{t1}|] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteTable T1
              t1 = writeTable
                [ deprecated
                , deprecated
                ]

              data U1
                = U1T1 !(Table T1)

              class WriteU1 a where
                u1 :: WriteTable a -> WriteUnion U1

              instance WriteU1 T1 where
                u1 = writeUnion 1

              readU1 :: forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union U1)
              readU1 n pos =
                case getPositive n of
                  1  -> Union . U1T1 <$> readTable pos
                  n' -> pure $! UnionUnknown n'
            |]

        it "required" $
          [r| table t1 {x: u1 (required) ;} union u1{t1}|] `shouldCompileTo`
            [d|
              data T1
              t1 :: WriteUnion U1 -> WriteTable T1
              t1 x = writeTable
                [ writeUnionType x
                , writeUnionValue x
                ]

              t1X :: forall m. ReadCtx m => Table T1 -> m (Union U1)
              t1X = readTableFieldUnion readU1 1

              data U1
                = U1T1 !(Table T1)

              class WriteU1 a where
                u1 :: WriteTable a -> WriteUnion U1

              instance WriteU1 T1 where
                u1 = writeUnion 1

              readU1 :: forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union U1)
              readU1 n pos =
                case getPositive n of
                  1  -> Union . U1T1 <$> readTable pos
                  n' -> pure $! UnionUnknown n'
            |]

    describe "Enums" $
      describe "naming conventions" $
        it "enum name / enum value names are uppercased" $
          [r|
            enum color: int16 { red = -2, Green, bLUE = 3  }

          |] `shouldCompileTo`
            [d|
              data Color = ColorRed | ColorGreen | ColorBLUE
                deriving (Eq, Show, Read, Ord, Bounded)

              toColor :: Int16 -> Maybe Color
              toColor n =
                case n of
                  -2 -> Just ColorRed
                  -1 -> Just ColorGreen
                  3 -> Just ColorBLUE
                  _ -> Nothing

              fromColor :: Color -> Int16
              fromColor n =
                case n of
                  ColorRed -> -2
                  ColorGreen -> -1
                  ColorBLUE -> 3
            |]

    describe "Structs" $ do
      describe "naming conventions" $
        it "struct name is uppercased, field names are lowercased" $
          [r|
            struct s { X: int; }
          |] `shouldCompileTo`
            [d|
              data S
              s :: Int32 -> WriteStruct S
              s x = writeStruct 4 [int32 x]

              sX :: forall m. ReadCtx m => Struct S -> m Int32
              sX = readStructField readInt32 0
            |]

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
              writeStruct 8
                [ padded 7 (bool     k)
                ,          (double   j)
                , padded 4 (float    i)
                ,          (int64    h)
                ,          (int32    g)
                ,          (int16    f)
                , padded 1 (int8     e)
                ,          (word64   d)
                ,          (word32   c)
                ,          (word16   b)
                , padded 1 (word8    a)
                ]

            scalarsA :: forall m. ReadCtx m => Struct Scalars -> m Word8
            scalarsA = readStructField readWord8 0
            scalarsB :: forall m. ReadCtx m => Struct Scalars -> m Word16
            scalarsB = readStructField readWord16 2
            scalarsC :: forall m. ReadCtx m => Struct Scalars -> m Word32
            scalarsC = readStructField readWord32 4
            scalarsD :: forall m. ReadCtx m => Struct Scalars -> m Word64
            scalarsD = readStructField readWord64 8
            scalarsE :: forall m. ReadCtx m => Struct Scalars -> m Int8
            scalarsE = readStructField readInt8 16
            scalarsF :: forall m. ReadCtx m => Struct Scalars -> m Int16
            scalarsF = readStructField readInt16 18
            scalarsG :: forall m. ReadCtx m => Struct Scalars -> m Int32
            scalarsG = readStructField readInt32 20
            scalarsH :: forall m. ReadCtx m => Struct Scalars -> m Int64
            scalarsH = readStructField readInt64 24
            scalarsI :: forall m. ReadCtx m => Struct Scalars -> m Float
            scalarsI = readStructField readFloat 32
            scalarsJ :: forall m. ReadCtx m => Struct Scalars -> m Double
            scalarsJ = readStructField readDouble 40
            scalarsK :: forall m. ReadCtx m => Struct Scalars -> m Bool
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

            fromE :: E -> Int8
            fromE n = case n of EX -> 0

            data S
            s :: Int8 -> WriteStruct S
            s e = writeStruct 1 [int8 e]

            sE :: forall m. ReadCtx m => Struct S -> m Int8
            sE = readStructField readInt8 0
          |]

      it "with nested structs" $
        [r|
          struct S1 (force_align: 2) { s2: S2; }
          struct S2 { x: int8; }
        |] `shouldCompileTo`
          [d|
            data S1
            s1 :: WriteStruct S2 -> WriteStruct S1
            s1 s2 = writeStruct 2 [padded 1 (unWriteStruct s2)]

            s1S2 :: Struct S1 -> Struct S2
            s1S2 = readStructField readStruct 0

            data S2
            s2 :: Int8 -> WriteStruct S2
            s2 x = writeStruct 1 [int8 x]

            s2X :: forall m. ReadCtx m => Struct S2 -> m Int8
            s2X = readStructField readInt8 0
          |]

    it "Unions" $
      [r|
        table t1{}
        table t2{}
        union weapon { t1, alias: t2 }
      |] `shouldCompileTo`
        [d|
          data T1
          t1 :: WriteTable T1
          t1 = writeTable []
          data T2
          t2 :: WriteTable T2
          t2 = writeTable []

          data Weapon
            = WeaponT1 !(Table T1)
            | WeaponAlias !(Table T2)

          class WriteWeapon a where
            weapon :: WriteTable a -> WriteUnion Weapon

          instance WriteWeapon T1 where
            weapon = writeUnion 1

          instance WriteWeapon T2 where
            weapon = writeUnion 2

          readWeapon :: forall m. ReadCtx m => Positive Word8 -> PositionInfo -> m (Union Weapon)
          readWeapon n pos =
            case getPositive n of
              1  -> Union . WeaponT1 <$> readTable pos
              2  -> Union . WeaponAlias <$> readTable pos
              n' -> pure $! UnionUnknown n'
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
          PrettyAst (normalizeDec <$> ast) `shouldBe` PrettyAst (normalizeDec <$> expected)

newtype PrettyAst = PrettyAst [Dec]
  deriving Eq

instance Show PrettyAst where
  show (PrettyAst decs) =
    let LitE (StringL s) = unsafePerformIO . runQ . simplifiedTH $ decs
    in  s

showBundle :: (ShowErrorComponent e, Stream s) => ParseErrorBundle s e -> String
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
    _ -> dec

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

normalizeTyVarBndr :: TyVarBndr -> TyVarBndr
normalizeTyVarBndr tv =
  case tv of
    PlainTV n -> PlainTV (normalizeName n)
    KindedTV n k -> KindedTV (normalizeName n) (normalizeType k)

normalizeClause :: Clause -> Clause
normalizeClause (Clause pats body decs) = Clause (normalizePat <$> pats) (normalizeBody body) (normalizeDec <$> decs)

normalizePat :: Pat -> Pat
normalizePat p =
  case p of
    VarP n -> VarP (normalizeName n)
    ConP n pats -> ConP (normalizeName n) (normalizePat <$> pats)
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
    CaseE e matches -> CaseE (normalizeExp e) (normalizeMatch <$> matches)
    ConE name -> ConE (normalizeName name)
    InfixE l op r -> InfixE (normalizeExp <$> l) (normalizeExp op) (normalizeExp <$> r)
    _ -> e

normalizeMatch :: Match -> Match
normalizeMatch (Match pat body decs) =
  Match (normalizePat pat) (normalizeBody body) (normalizeDec <$> decs)

normalizeName (Name (OccName occ) (NameU _)) = mkName occ
normalizeName name = name

