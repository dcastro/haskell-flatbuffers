{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module FlatBuffers.Internal.Compiler.TH where

import           Control.Monad                                   ( join )
import           Control.Monad.Except                            ( runExceptT )

import           Data.Foldable                                   ( traverse_ )
import           Data.Functor                                    ( (<&>) )
import           Data.Int
import qualified Data.List                                       as List
import           Data.List.NonEmpty                              ( NonEmpty(..) )
import qualified Data.List.NonEmpty                              as NE
import qualified Data.Map.Strict                                 as Map
import           Data.Text                                       ( Text )
import qualified Data.Text                                       as T
import           Data.Word

import           FlatBuffers.Internal.Build
import qualified FlatBuffers.Internal.Compiler.NamingConventions as NC
import qualified FlatBuffers.Internal.Compiler.ParserIO          as ParserIO
import           FlatBuffers.Internal.Compiler.SemanticAnalysis  ( SymbolTable(..) )
import qualified FlatBuffers.Internal.Compiler.SemanticAnalysis  as SemanticAnalysis
import qualified FlatBuffers.Internal.Compiler.SyntaxTree        as SyntaxTree
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree
import           FlatBuffers.Internal.FileIdentifier             ( HasFileIdentifier(..), unsafeFileIdentifier )
import           FlatBuffers.Internal.Read
import           FlatBuffers.Internal.Types
import           FlatBuffers.Internal.Write

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax                      ( lift )
import qualified Language.Haskell.TH.Syntax                      as TH


-- | Helper method to create function types.
-- @ConT ''Int ~> ConT ''String === Int -> String@
(~>) :: Type -> Type -> Type
a ~> b = ArrowT `AppT` a `AppT` b
infixr 1 ~>

-- | Options to control how\/which flatbuffers constructors\/accessor should be generated.
--
-- Options can be set using record syntax on `defaultOptions` with the fields below.
--
-- > defaultOptions { compileAllSchemas = True }
data Options = Options
  { -- | Directories to search for @include@s (same as flatc @-I@ option).
    includeDirectories :: [FilePath]
    -- | Generate code not just for the root schema,
    -- but for all schemas it includes as well
    -- (same as flatc @--gen-all@ option).
  , compileAllSchemas :: Bool
  }
  deriving (Show, Eq)

-- | Default flatbuffers options:
--
-- > Options
-- >   { includeDirectories = []
-- >   , compileAllSchemas = False
-- >   }
defaultOptions :: Options
defaultOptions = Options
  { includeDirectories = []
  , compileAllSchemas = False
  }

-- | Generates constructors and accessors for all data types declared in the given flatbuffers
-- schema whose namespace matches the current module.
--
-- > namespace Data.Game;
-- >
-- > table Monster {}
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > module Data.Game where
-- > import FlatBuffers
-- >
-- > $(mkFlatBuffers "schemas/game.fbs" defaultOptions)
mkFlatBuffers :: FilePath -> Options -> Q [Dec]
mkFlatBuffers rootFilePath opts = do
  currentModule <- T.pack . loc_module <$> location

  parseResult <- runIO $ runExceptT $ ParserIO.parseSchemas rootFilePath (includeDirectories opts)

  schemaFileTree <- either (fail . fixMsg . T.unpack) pure parseResult

  registerFiles schemaFileTree

  symbolTables <- either (fail . fixMsg . T.unpack) pure $ SemanticAnalysis.validateSchemas schemaFileTree

  let symbolTable =
        if compileAllSchemas opts
          then SyntaxTree.fileTreeRoot symbolTables
                <> mconcat (Map.elems $ SyntaxTree.fileTreeForest symbolTables)
          else SyntaxTree.fileTreeRoot symbolTables

  let symbolTable' = filterByCurrentModule currentModule symbolTable

  compileSymbolTable symbolTable'

  where
    registerFiles (SyntaxTree.FileTree rootFilePath _ includedFiles) = do
      TH.addDependentFile rootFilePath
      traverse_ TH.addDependentFile $ Map.keys includedFiles

    filterByCurrentModule currentModule (SymbolTable enums structs tables unions) =
      SymbolTable
        { allEnums   = filter (isCurrentModule currentModule) enums
        , allStructs = filter (isCurrentModule currentModule) structs
        , allTables  = filter (isCurrentModule currentModule) tables
        , allUnions  = filter (isCurrentModule currentModule) unions
        }

    isCurrentModule currentModule (ns, _) = NC.namespace ns == currentModule

-- | This does two things:
--
-- 1. ghcid stops parsing an error when it finds a line that start with alphabetical characters or an empty lines,
--    so we prepend each line with an empty space to avoid this.
-- 2. we also remove any trailing \n, otherwise ghcid would stop parsing here and not show the source code location.
fixMsg :: String -> String
fixMsg = List.intercalate "\n" . fmap fixLine . lines
  where
    fixLine line = " " <> line

compileSymbolTable :: SemanticAnalysis.ValidDecls -> Q [Dec]
compileSymbolTable symbolTable = do
  enumDecs <- join <$> traverse mkEnum (allEnums symbolTable)
  structDecs <- join <$> traverse mkStruct (allStructs symbolTable)
  tableDecs <- join <$> traverse mkTable (allTables symbolTable)
  unionDecs <- join <$> traverse mkUnion (allUnions symbolTable)
  pure $ enumDecs <> structDecs <> tableDecs <> unionDecs

mkEnum :: (Namespace, EnumDecl) -> Q [Dec]
mkEnum (_, enum) = do
  let enumName = mkName' $ NC.dataTypeName enum

  let enumValNames = enumVals enum <&> \enumVal ->
        mkName $ T.unpack $ NC.enumUnionMember enum enumVal

  let enumDec = mkEnumDataDec enumName enumValNames
  toEnumDecs <- mkToEnum enumName enum (enumVals enum `NE.zip` enumValNames)
  fromEnumDecs <- mkFromEnum enumName enum (enumVals enum `NE.zip` enumValNames)

  pure $ enumDec : toEnumDecs <> fromEnumDecs

mkEnumDataDec :: Name -> NonEmpty Name -> Dec
mkEnumDataDec enumName enumValNames =
  DataD [] enumName [] Nothing
    (NE.toList $ fmap (\n -> NormalC n []) enumValNames)
    [ DerivClause Nothing
      [ ConT ''Eq
      , ConT ''Show
      , ConT ''Read
      , ConT ''Ord
      , ConT ''Bounded
      ]
    ]

mkToEnum :: Name -> EnumDecl -> NonEmpty (EnumVal, Name) -> Q [Dec]
mkToEnum enumName enum enumValsAndNames = do
  let funName = mkName' $ NC.toEnumFun enum
  argName <- newName "n"
  pure
    [ SigD funName (enumTypeToType (enumType enum) ~> ConT ''Maybe `AppT` ConT enumName)
    , FunD funName
      [ Clause
        [VarP argName]
        (NormalB (CaseE (VarE argName) matches))
        []
      ]
    , PragmaD $ InlineP funName Inline FunLike AllPhases
    ]
  where
    matches =
      (mkMatch <$> NE.toList enumValsAndNames) <> [matchWildcard]

    mkMatch (enumVal, enumName) =
      Match
        (intLitP (enumValInt enumVal))
        (NormalB (ConE 'Just `AppE` ConE enumName))
        []

    matchWildcard =
      Match
        WildP
        (NormalB (ConE 'Nothing))
        []

mkFromEnum :: Name -> EnumDecl -> NonEmpty (EnumVal, Name) -> Q [Dec]
mkFromEnum enumName enum enumValsAndNames = do
  let funName = mkName' $ NC.fromEnumFun enum
  argName <- newName "n"
  pure
    [ SigD funName (ConT enumName ~> enumTypeToType (enumType enum))
    , FunD funName
      [ Clause
        [VarP argName]
        (NormalB (CaseE (VarE argName) (mkMatch <$> NE.toList enumValsAndNames)))
        []
      ]
    , PragmaD $ InlineP funName Inline FunLike AllPhases
    ]
  where
    mkMatch (enumVal, enumName) =
      Match
        (ConP enumName [])
        (NormalB (intLitE (enumValInt enumVal)))
        []


mkStruct :: (Namespace, StructDecl) -> Q [Dec]
mkStruct (_, struct) = do
  let structName = mkName' $ NC.dataTypeName struct
  isStructInstance <- mkIsStructInstance structName struct

  let dataDec = DataD [] structName [] Nothing [] []
  (consSig, cons) <- mkStructConstructor structName struct

  let getters = foldMap (mkStructFieldGetter structName struct) (structFields struct)

  pure $
    dataDec :
    isStructInstance <>
    [ consSig, cons ] <>
    getters

mkIsStructInstance :: Name -> StructDecl -> Q [Dec]
mkIsStructInstance structName struct =
  [d|
    instance IsStruct $(conT structName) where
      structAlignmentOf = $(lift . unAlignment  . structAlignment $ struct)
      structSizeOf      = $(lift . unInlineSize . structSize      $ struct)
  |]

mkStructConstructor :: Name -> StructDecl -> Q (Dec, Dec)
mkStructConstructor structName struct = do
  argsInfo <- traverse mkStructConstructorArg (structFields struct)
  let (argTypes, pats, exps) = nonEmptyUnzip3 argsInfo

  let retType = AppT (ConT ''WriteStruct) (ConT structName)
  let sigType = foldr (~>) retType argTypes

  let consName = mkName' $ NC.dataTypeConstructor struct
  let consSig = SigD consName sigType

  let exp = foldr1 (\e acc -> InfixE (Just e) (VarE '(<>)) (Just acc)) (join exps)
  let body = NormalB $ ConE 'WriteStruct `AppE` exp

  let cons = FunD consName [ Clause (NE.toList pats) body [] ]

  pure (consSig, cons)


mkStructConstructorArg :: StructField -> Q (Type, Pat, NonEmpty Exp)
mkStructConstructorArg sf = do
  argName <- newName' $ NC.arg sf
  let argPat = VarP argName
  let argRef = VarE argName
  let argType = structFieldTypeToWriteType (structFieldType sf)

  let mkWriteExp sft =
        case sft of
          SInt8            -> VarE 'buildInt8
          SInt16           -> VarE 'buildInt16
          SInt32           -> VarE 'buildInt32
          SInt64           -> VarE 'buildInt64
          SWord8           -> VarE 'buildWord8
          SWord16          -> VarE 'buildWord16
          SWord32          -> VarE 'buildWord32
          SWord64          -> VarE 'buildWord64
          SFloat           -> VarE 'buildFloat
          SDouble          -> VarE 'buildDouble
          SBool            -> VarE 'buildBool
          SEnum _ enumType -> mkWriteExp (enumTypeToStructFieldType enumType)
          SStruct _        -> VarE 'buildStruct

  let exp = mkWriteExp (structFieldType sf) `AppE` argRef

  let exps =
        if structFieldPadding sf == 0
          then [ exp ]
          else
            [ exp
            , VarE 'buildPadding `AppE` intLitE (structFieldPadding sf)
            ]

  pure (argType, argPat, exps)

mkStructFieldGetter :: Name -> StructDecl -> StructField -> [Dec]
mkStructFieldGetter structName struct sf =
  [sig, fun]
  where
    funName = mkName (T.unpack (NC.getter struct sf))
    fieldOffsetExp = intLitE (structFieldOffset sf)

    retType = structFieldTypeToReadType (structFieldType sf)
    sig =
      SigD funName $
        case structFieldType sf of
          SStruct _ ->
            ConT ''Struct `AppT` ConT structName ~> retType
          _ ->
            ConT ''Struct `AppT` ConT structName ~> ConT ''Either `AppT` ConT ''ReadError `AppT` retType

    fun = FunD funName [ Clause [] (NormalB body) [] ]

    body = app
      [ VarE 'readStructField
      , mkReadExp (structFieldType sf)
      , fieldOffsetExp
      ]

    mkReadExp sft =
      case sft of
        SInt8   -> VarE 'readInt8
        SInt16  -> VarE 'readInt16
        SInt32  -> VarE 'readInt32
        SInt64  -> VarE 'readInt64
        SWord8  -> VarE 'readWord8
        SWord16 -> VarE 'readWord16
        SWord32 -> VarE 'readWord32
        SWord64 -> VarE 'readWord64
        SFloat  -> VarE 'readFloat
        SDouble -> VarE 'readDouble
        SBool   -> VarE 'readBool
        SEnum _ enumType -> mkReadExp $ enumTypeToStructFieldType enumType
        SStruct _ -> VarE 'readStruct

mkTable :: (Namespace, TableDecl) -> Q [Dec]
mkTable (_, table) = do
  let tableName = mkName' $ NC.dataTypeName table
  (consSig, cons) <- mkTableConstructor tableName table

  let fileIdentifierDec = mkTableFileIdentifier tableName (tableIsRoot table)
  let getters = foldMap (mkTableFieldGetter tableName table) (tableFields table)

  pure $
    [ DataD [] tableName [] Nothing [] []
    , consSig
    , cons
    ] <> fileIdentifierDec
    <> getters

mkTableFileIdentifier :: Name -> IsRoot -> [Dec]
mkTableFileIdentifier tableName isRoot =
  case isRoot of
    NotRoot -> []
    IsRoot Nothing -> []
    IsRoot (Just fileIdentifier) ->
      [ InstanceD
          Nothing
          []
          (ConT ''HasFileIdentifier `AppT` ConT tableName)
          [ FunD 'getFileIdentifier
            [ Clause
              []
              (NormalB $ VarE 'unsafeFileIdentifier `AppE` textLitE fileIdentifier)
              []
            ]
          ]
      ]

mkTableConstructor :: Name -> TableDecl -> Q (Dec, Dec)
mkTableConstructor tableName table = do
  (argTypes, pats, exps) <- mconcat <$> traverse mkTableContructorArg (tableFields table)

  let retType = AppT (ConT ''WriteTable) (ConT tableName)
  let sigType = foldr (~>) retType argTypes

  let consName = mkName' $ NC.dataTypeConstructor table
  let consSig = SigD consName sigType

  let body = NormalB $ AppE (VarE 'writeTable) (ListE exps)
  let cons = FunD consName [ Clause pats body [] ]

  pure (consSig, cons)

mkTableContructorArg :: TableField -> Q ([Type], [Pat], [Exp])
mkTableContructorArg tf =
  if tableFieldDeprecated tf
    then
      case tableFieldType tf of
        TUnion _ _           -> pure ([], [], [VarE 'deprecated, VarE 'deprecated])
        TVector _ (VUnion _) -> pure ([], [], [VarE 'deprecated, VarE 'deprecated])
        _                    -> pure ([], [], [VarE 'deprecated])
    else do
      argName <- newName' $ NC.arg tf
      let argPat = VarP argName
      let argRef = VarE argName
      let argType = tableFieldTypeToWriteType (tableFieldType tf)
      let exps = mkExps argRef (tableFieldType tf)

      pure ([argType], [argPat], exps)

  where
    expForScalar :: Exp -> Exp -> Exp -> Exp
    expForScalar defaultValExp writeExp varExp =
      VarE 'optionalDef `AppE` defaultValExp `AppE` writeExp `AppE` varExp

    expForNonScalar :: Required -> Exp -> Exp -> Exp
    expForNonScalar Req exp argRef = exp `AppE` argRef
    expForNonScalar Opt exp argRef = VarE 'optional `AppE` exp `AppE` argRef

    mkExps :: Exp -> TableFieldType -> [Exp]
    mkExps argRef tfType =
        case tfType of
          TInt8   (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeInt8TableField   ) argRef
          TInt16  (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeInt16TableField  ) argRef
          TInt32  (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeInt32TableField  ) argRef
          TInt64  (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeInt64TableField  ) argRef
          TWord8  (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeWord8TableField  ) argRef
          TWord16 (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeWord16TableField ) argRef
          TWord32 (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeWord32TableField ) argRef
          TWord64 (DefaultVal n) -> pure $ expForScalar (intLitE n)  (VarE 'writeWord64TableField ) argRef
          TFloat  (DefaultVal n) -> pure $ expForScalar (realLitE n) (VarE 'writeFloatTableField  ) argRef
          TDouble (DefaultVal n) -> pure $ expForScalar (realLitE n) (VarE 'writeDoubleTableField ) argRef
          TBool   (DefaultVal b) -> pure $ expForScalar (if b then ConE 'True else ConE 'False)  (VarE 'writeBoolTableField) argRef
          TString req            -> pure $ expForNonScalar req (VarE 'writeTextTableField) argRef
          TEnum _ enumType dflt  -> mkExps argRef (enumTypeToTableFieldType enumType dflt)
          TStruct _ req          -> pure $ expForNonScalar req (VarE 'writeStructTableField) argRef
          TTable _ req           -> pure $ expForNonScalar req (VarE 'writeTableTableField) argRef
          TUnion _ _             ->
            [ VarE 'writeUnionTypeTableField `AppE` argRef
            , VarE 'writeUnionValueTableField `AppE` argRef
            ]
          TVector req vecElemType -> mkExpForVector argRef req vecElemType

    mkExpForVector :: Exp -> Required -> VectorElementType -> [Exp]
    mkExpForVector argRef req vecElemType =
        case vecElemType of
          VInt8            -> [ expForNonScalar req (VarE 'writeVectorInt8TableField) argRef ]
          VInt16           -> [ expForNonScalar req (VarE 'writeVectorInt16TableField) argRef ]
          VInt32           -> [ expForNonScalar req (VarE 'writeVectorInt32TableField) argRef ]
          VInt64           -> [ expForNonScalar req (VarE 'writeVectorInt64TableField) argRef ]
          VWord8           -> [ expForNonScalar req (VarE 'writeVectorWord8TableField) argRef ]
          VWord16          -> [ expForNonScalar req (VarE 'writeVectorWord16TableField) argRef ]
          VWord32          -> [ expForNonScalar req (VarE 'writeVectorWord32TableField) argRef ]
          VWord64          -> [ expForNonScalar req (VarE 'writeVectorWord64TableField) argRef ]
          VFloat           -> [ expForNonScalar req (VarE 'writeVectorFloatTableField) argRef ]
          VDouble          -> [ expForNonScalar req (VarE 'writeVectorDoubleTableField) argRef ]
          VBool            -> [ expForNonScalar req (VarE 'writeVectorBoolTableField) argRef ]
          VString          -> [ expForNonScalar req (VarE 'writeVectorTextTableField) argRef ]
          VEnum _ enumType -> mkExpForVector argRef req (enumTypeToVectorElementType enumType)
          VStruct _        -> [ expForNonScalar req (VarE 'writeVectorStructTableField) argRef ]
          VTable _         -> [ expForNonScalar req (VarE 'writeVectorTableTableField) argRef ]
          VUnion _ ->
            [ expForNonScalar req (VarE 'writeUnionTypesVectorTableField) argRef
            , expForNonScalar req (VarE 'writeUnionValuesVectorTableField) argRef
            ]

mkTableFieldGetter :: Name -> TableDecl -> TableField -> [Dec]
mkTableFieldGetter tableName table tf =
  if tableFieldDeprecated tf
    then []
    else [sig, mkFun (tableFieldType tf)]
  where
    funName = mkName (T.unpack (NC.getter table tf))
    fieldIndex = intLitE (tableFieldId tf)

    sig =
      SigD funName $
        ConT ''Table `AppT` ConT tableName ~> ConT ''Either `AppT` ConT ''ReadError `AppT` tableFieldTypeToReadType (tableFieldType tf)

    mkFun :: TableFieldType -> Dec
    mkFun tft =
      case tft of
        TWord8 (DefaultVal n)   -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readWord8))
        TWord16 (DefaultVal n)  -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readWord16))
        TWord32 (DefaultVal n)  -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readWord32))
        TWord64 (DefaultVal n)  -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readWord64))
        TInt8 (DefaultVal n)    -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readInt8))
        TInt16 (DefaultVal n)   -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readInt16))
        TInt32 (DefaultVal n)   -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readInt32))
        TInt64 (DefaultVal n)   -> mkFunWithBody (bodyForScalar (intLitE n)   (VarE 'readInt64))
        TFloat (DefaultVal n)   -> mkFunWithBody (bodyForScalar (realLitE n)  (VarE 'readFloat))
        TDouble (DefaultVal n)  -> mkFunWithBody (bodyForScalar (realLitE n)  (VarE 'readDouble))
        TBool (DefaultVal b)    -> mkFunWithBody (bodyForScalar (if b then ConE 'True else ConE 'False) (VarE 'readBool))
        TString req             -> mkFunWithBody (bodyForNonScalar req (VarE 'readText))
        TEnum _ enumType dflt   -> mkFun $ enumTypeToTableFieldType enumType dflt
        TStruct _ req           -> mkFunWithBody (bodyForNonScalar req (compose [ConE 'Right, VarE 'readStruct]))
        TTable _ req            -> mkFunWithBody (bodyForNonScalar req (VarE 'readTable))
        TUnion (TypeRef ns ident) _req ->
          mkFunWithBody $ app
            [ VarE 'readTableFieldUnion
            , VarE . mkName . T.unpack . NC.withModulePrefix ns $ NC.readUnionFun ident
            , fieldIndex
            ]
        TVector req vecElemType -> mkFunForVector req vecElemType

    mkFunForVector :: Required -> VectorElementType -> Dec
    mkFunForVector req vecElemType =
      case vecElemType of
        VInt8            -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorInt8
        VInt16           -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorInt16
        VInt32           -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorInt32
        VInt64           -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorInt64
        VWord8           -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorWord8
        VWord16          -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorWord16
        VWord32          -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorWord32
        VWord64          -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorWord64
        VFloat           -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorFloat
        VDouble          -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorDouble
        VBool            -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorBool
        VString          -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorText
        VEnum _ enumType -> mkFunForVector req (enumTypeToVectorElementType enumType)
        VStruct _        -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'VectorStruct
        VTable _         -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readTableVector
        VUnion (TypeRef ns ident) ->
          mkFunWithBody $
            case req of
              Opt -> app
                [ VarE 'readTableFieldUnionVectorOpt
                , VarE . mkName . T.unpack . NC.withModulePrefix ns $ NC.readUnionFun ident
                , fieldIndex
                ]
              Req -> app
                [ VarE 'readTableFieldUnionVectorReq
                , VarE . mkName . T.unpack . NC.withModulePrefix ns $ NC.readUnionFun ident
                , fieldIndex
                , stringLitE . unIdent . getIdent $ tf
                ]


    mkFunWithBody :: Exp -> Dec
    mkFunWithBody body = FunD funName [ Clause [] (NormalB body) [] ]

    bodyForNonScalar req readExp =
      case req of
        Req ->
          app
            [ VarE 'readTableFieldReq
            , readExp
            , fieldIndex
            , stringLitE . unIdent . getIdent $ tf
            ]
        Opt ->
          app
            [ VarE 'readTableFieldOpt
            , readExp
            , fieldIndex
            ]

    bodyForScalar defaultValExp readExp =
      app
        [ VarE 'readTableFieldWithDef
        , readExp
        , fieldIndex
        , defaultValExp
        ]

mkUnion :: (Namespace, UnionDecl) -> Q [Dec]
mkUnion (_, union) = do
  let unionName = mkName' $ NC.dataTypeName union
  let unionValNames = unionVals union <&> \unionVal ->
        mkName $ T.unpack $ NC.enumUnionMember union unionVal

  unionConstructors <- mkUnionConstructors unionName union

  readFun <- mkReadUnionFun unionName unionValNames union

  pure $
    mkUnionDataDec unionName (unionVals union `NE.zip` unionValNames)
    : unionConstructors
    <> readFun


mkUnionDataDec :: Name -> NonEmpty (UnionVal, Name) -> Dec
mkUnionDataDec unionName unionValsAndNames =
  DataD [] unionName [] Nothing
    (NE.toList $ fmap mkCons unionValsAndNames)
    []
  where
    mkCons (unionVal, unionValName) =
      NormalC unionValName [(bang, ConT ''Table `AppT` typeRefToType (unionValTableRef unionVal))]

    bang = Bang NoSourceUnpackedness SourceStrict

mkUnionConstructors :: Name -> UnionDecl -> Q [Dec]
mkUnionConstructors unionName union =
  fmap join . traverse mkUnionConstructor $ NE.toList (unionVals union) `zip` [1..]
  where
    mkUnionConstructor :: (UnionVal, Integer) -> Q [Dec]
    mkUnionConstructor (unionVal, ix) = do
      let constructorName = mkName' $ NC.unionConstructor union unionVal
      pure
        [ SigD constructorName $
          ConT ''WriteTable `AppT` typeRefToType (unionValTableRef unionVal)
            ~> ConT ''WriteUnion `AppT` ConT unionName
        , FunD constructorName
          [ Clause
            []
            (NormalB $ VarE 'writeUnion `AppE` intLitE ix)
            []
          ]
        ]

mkReadUnionFun :: Name -> NonEmpty Name -> UnionDecl -> Q [Dec]
mkReadUnionFun unionName unionValNames union = do
  nArg <- newName "n"
  posArg <- newName "pos"
  wildcard <- newName "n'"

  let funName = mkName $ T.unpack $ NC.readUnionFun union
  let sig =
        SigD funName $
          ConT ''Positive `AppT` ConT ''Word8
            ~> ConT ''PositionInfo
            ~> ConT ''Either `AppT` ConT ''ReadError `AppT` (ConT ''Union `AppT` ConT unionName)

  let
    mkMatch :: Name -> Integer -> Match
    mkMatch unionValName ix =
      Match
        (intLitP ix)
        (NormalB $
          InfixE
            (Just (compose [ConE 'Union, ConE unionValName]))
            (VarE '(<$>))
            (Just (VarE 'readTable' `AppE` VarE posArg))
        )
        []

  let matchWildcard =
        Match
          (VarP wildcard)
          (NormalB $
            InfixE
              (Just (VarE 'pure))
              (VarE '($!))
              (Just (ConE 'UnionUnknown `AppE` VarE wildcard))
          )
          []

  let matches = (uncurry mkMatch <$> NE.toList unionValNames `zip` [1..]) <> [matchWildcard]

  let funBody =
        NormalB $
          CaseE
            (VarE 'getPositive `AppE` VarE nArg)
            matches

  let fun =
        FunD funName
          [ Clause
              [VarP nArg, VarP posArg]
              funBody
              []
          ]
  pure [sig, fun]

enumTypeToType :: EnumType -> Type
enumTypeToType et =
  case et of
    EInt8   -> ConT ''Int8
    EInt16  -> ConT ''Int16
    EInt32  -> ConT ''Int32
    EInt64  -> ConT ''Int64
    EWord8  -> ConT ''Word8
    EWord16 -> ConT ''Word16
    EWord32 -> ConT ''Word32
    EWord64 -> ConT ''Word64

enumTypeToTableFieldType :: Integral a => EnumType -> DefaultVal a -> TableFieldType
enumTypeToTableFieldType et dflt =
  case et of
    EInt8   -> TInt8 (fromIntegral dflt)
    EInt16  -> TInt16 (fromIntegral dflt)
    EInt32  -> TInt32 (fromIntegral dflt)
    EInt64  -> TInt64 (fromIntegral dflt)
    EWord8  -> TWord8 (fromIntegral dflt)
    EWord16 -> TWord16 (fromIntegral dflt)
    EWord32 -> TWord32 (fromIntegral dflt)
    EWord64 -> TWord64 (fromIntegral dflt)

enumTypeToStructFieldType :: EnumType -> StructFieldType
enumTypeToStructFieldType et =
  case et of
    EInt8   -> SInt8
    EInt16  -> SInt16
    EInt32  -> SInt32
    EInt64  -> SInt64
    EWord8  -> SWord8
    EWord16 -> SWord16
    EWord32 -> SWord32
    EWord64 -> SWord64

enumTypeToVectorElementType :: EnumType -> VectorElementType
enumTypeToVectorElementType et =
  case et of
    EInt8   -> VInt8
    EInt16  -> VInt16
    EInt32  -> VInt32
    EInt64  -> VInt64
    EWord8  -> VWord8
    EWord16 -> VWord16
    EWord32 -> VWord32
    EWord64 -> VWord64

structFieldTypeToWriteType :: StructFieldType -> Type
structFieldTypeToWriteType sft =
  case sft of
    SInt8   -> ConT ''Int8
    SInt16  -> ConT ''Int16
    SInt32  -> ConT ''Int32
    SInt64  -> ConT ''Int64
    SWord8  -> ConT ''Word8
    SWord16 -> ConT ''Word16
    SWord32 -> ConT ''Word32
    SWord64 -> ConT ''Word64
    SFloat  -> ConT ''Float
    SDouble -> ConT ''Double
    SBool   -> ConT ''Bool
    SEnum _ enumType -> enumTypeToType enumType
    SStruct (namespace, structDecl) ->
      ConT ''WriteStruct `AppT` typeRefToType (TypeRef namespace (getIdent structDecl))

structFieldTypeToReadType :: StructFieldType -> Type
structFieldTypeToReadType sft =
  case sft of
    SInt8   -> ConT ''Int8
    SInt16  -> ConT ''Int16
    SInt32  -> ConT ''Int32
    SInt64  -> ConT ''Int64
    SWord8  -> ConT ''Word8
    SWord16 -> ConT ''Word16
    SWord32 -> ConT ''Word32
    SWord64 -> ConT ''Word64
    SFloat  -> ConT ''Float
    SDouble -> ConT ''Double
    SBool   -> ConT ''Bool
    SEnum _ enumType -> enumTypeToType enumType
    SStruct (namespace, structDecl) ->
      ConT ''Struct `AppT` typeRefToType (TypeRef namespace (getIdent structDecl))

tableFieldTypeToWriteType :: TableFieldType -> Type
tableFieldTypeToWriteType tft =
  case tft of
    TInt8   _   -> ConT ''Maybe `AppT` ConT ''Int8
    TInt16  _   -> ConT ''Maybe `AppT` ConT ''Int16
    TInt32  _   -> ConT ''Maybe `AppT` ConT ''Int32
    TInt64  _   -> ConT ''Maybe `AppT` ConT ''Int64
    TWord8  _   -> ConT ''Maybe `AppT` ConT ''Word8
    TWord16 _   -> ConT ''Maybe `AppT` ConT ''Word16
    TWord32 _   -> ConT ''Maybe `AppT` ConT ''Word32
    TWord64 _   -> ConT ''Maybe `AppT` ConT ''Word64
    TFloat  _   -> ConT ''Maybe `AppT` ConT ''Float
    TDouble _   -> ConT ''Maybe `AppT` ConT ''Double
    TBool   _   -> ConT ''Maybe `AppT` ConT ''Bool
    TString req             -> requiredType req (ConT ''Text)
    TEnum _ enumType _      -> ConT ''Maybe `AppT` enumTypeToType enumType
    TStruct typeRef req     -> requiredType req (ConT ''WriteStruct `AppT` typeRefToType typeRef)
    TTable typeRef req      -> requiredType req (ConT ''WriteTable  `AppT` typeRefToType typeRef)
    TUnion typeRef _        -> ConT ''WriteUnion  `AppT` typeRefToType typeRef
    TVector req vecElemType -> requiredType req (vectorElementTypeToWriteType vecElemType)

tableFieldTypeToReadType :: TableFieldType -> Type
tableFieldTypeToReadType tft =
  case tft of
    TInt8   _   -> ConT ''Int8
    TInt16  _   -> ConT ''Int16
    TInt32  _   -> ConT ''Int32
    TInt64  _   -> ConT ''Int64
    TWord8  _   -> ConT ''Word8
    TWord16 _   -> ConT ''Word16
    TWord32 _   -> ConT ''Word32
    TWord64 _   -> ConT ''Word64
    TFloat  _   -> ConT ''Float
    TDouble _   -> ConT ''Double
    TBool   _   -> ConT ''Bool
    TString req             -> requiredType req (ConT ''Text)
    TEnum _ enumType _      -> enumTypeToType enumType
    TStruct typeRef req     -> requiredType req (ConT ''Struct `AppT` typeRefToType typeRef)
    TTable typeRef req      -> requiredType req (ConT ''Table  `AppT` typeRefToType typeRef)
    TUnion typeRef _        -> ConT ''Union  `AppT` typeRefToType typeRef
    TVector req vecElemType -> requiredType req (vectorElementTypeToReadType vecElemType)

vectorElementTypeToWriteType :: VectorElementType -> Type
vectorElementTypeToWriteType vet =
  case vet of
    VInt8                 -> ConT ''WriteVector `AppT` ConT ''Int8
    VInt16                -> ConT ''WriteVector `AppT` ConT ''Int16
    VInt32                -> ConT ''WriteVector `AppT` ConT ''Int32
    VInt64                -> ConT ''WriteVector `AppT` ConT ''Int64
    VWord8                -> ConT ''WriteVector `AppT` ConT ''Word8
    VWord16               -> ConT ''WriteVector `AppT` ConT ''Word16
    VWord32               -> ConT ''WriteVector `AppT` ConT ''Word32
    VWord64               -> ConT ''WriteVector `AppT` ConT ''Word64
    VFloat                -> ConT ''WriteVector `AppT` ConT ''Float
    VDouble               -> ConT ''WriteVector `AppT` ConT ''Double
    VBool                 -> ConT ''WriteVector `AppT` ConT ''Bool
    VString               -> ConT ''WriteVector `AppT` ConT ''Text
    VEnum   _ enumType    -> ConT ''WriteVector `AppT` enumTypeToType enumType
    VStruct typeRef       -> ConT ''WriteVector `AppT` (ConT ''WriteStruct `AppT` typeRefToType typeRef)
    VTable  typeRef       -> ConT ''WriteVector `AppT` (ConT ''WriteTable  `AppT` typeRefToType typeRef)
    VUnion  typeRef       -> ConT ''WriteVector `AppT` (ConT ''WriteUnion  `AppT` typeRefToType typeRef)

vectorElementTypeToReadType :: VectorElementType -> Type
vectorElementTypeToReadType vet =
  case vet of
    VInt8                 -> ConT ''Vector `AppT` ConT ''Int8
    VInt16                -> ConT ''Vector `AppT` ConT ''Int16
    VInt32                -> ConT ''Vector `AppT` ConT ''Int32
    VInt64                -> ConT ''Vector `AppT` ConT ''Int64
    VWord8                -> ConT ''Vector `AppT` ConT ''Word8
    VWord16               -> ConT ''Vector `AppT` ConT ''Word16
    VWord32               -> ConT ''Vector `AppT` ConT ''Word32
    VWord64               -> ConT ''Vector `AppT` ConT ''Word64
    VFloat                -> ConT ''Vector `AppT` ConT ''Float
    VDouble               -> ConT ''Vector `AppT` ConT ''Double
    VBool                 -> ConT ''Vector `AppT` ConT ''Bool
    VString               -> ConT ''Vector `AppT` ConT ''Text
    VEnum   _ enumType    -> ConT ''Vector `AppT` enumTypeToType enumType
    VStruct typeRef       -> ConT ''Vector `AppT` (ConT ''Struct `AppT` typeRefToType typeRef)
    VTable  typeRef       -> ConT ''Vector `AppT` (ConT ''Table  `AppT` typeRefToType typeRef)
    VUnion  typeRef       -> ConT ''Vector `AppT` (ConT ''Union  `AppT` typeRefToType typeRef)

typeRefToType :: TypeRef -> Type
typeRefToType (TypeRef ns ident) =
  ConT . mkName' . NC.withModulePrefix ns . NC.dataTypeName $ ident

requiredType :: Required -> Type -> Type
requiredType Req t = t
requiredType Opt t = AppT (ConT ''Maybe) t

mkName' :: Text -> Name
mkName' = mkName . T.unpack

newName' :: Text -> Q Name
newName' = newName . T.unpack


intLitP :: Integral i => i -> Pat
intLitP = LitP . IntegerL . toInteger

intLitE :: Integral i => i -> Exp
intLitE = LitE . IntegerL . toInteger

realLitE :: Real i => i -> Exp
realLitE = LitE . RationalL . toRational

textLitE :: Text -> Exp
textLitE t = VarE 'T.pack `AppE` LitE (StringL (T.unpack t))

stringLitE :: Text -> Exp
stringLitE t = LitE (StringL (T.unpack t))

-- | Applies a function to multiple arguments. Assumes the list is not empty.
app :: [Exp] -> Exp
app = foldl1 AppE

compose :: [Exp] -> Exp
compose = foldr1 (\e1 e2 -> InfixE (Just e1) (VarE '(.)) (Just e2))


nonEmptyUnzip3 :: NonEmpty (a,b,c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
nonEmptyUnzip3 xs =
  ( (\(x, _, _) -> x) <$> xs
  , (\(_, x, _) -> x) <$> xs
  , (\(_, _, x) -> x) <$> xs
  )
