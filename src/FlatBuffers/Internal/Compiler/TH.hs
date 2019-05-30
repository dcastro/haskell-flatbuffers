{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FlatBuffers.Internal.Compiler.TH where

import           Control.Monad                                   ( forM, join )

import           Data.Int
import qualified Data.List                                       as List
import           Data.List.NonEmpty                              ( NonEmpty(..) )
import qualified Data.List.NonEmpty                              as NE
import           Data.Text                                       ( Text )
import qualified Data.Text                                       as T
import           Data.Word

import qualified FlatBuffers.Internal.Compiler.NamingConventions as NC
import           FlatBuffers.Internal.Compiler.SemanticAnalysis  ( SymbolTable(..) )
import           FlatBuffers.Internal.Compiler.SyntaxTree        ( HasIdent(..), Ident(..), Namespace(..), TypeRef(..) )
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree
import           FlatBuffers.Internal.Positive                   ( Positive(getPositive) )
import           FlatBuffers.Internal.Util                       ( nonEmptyUnzip3 )
import           FlatBuffers.Read
import           FlatBuffers.Write

import           Language.Haskell.TH

-- | Helper method to create function types.
-- @ConT ''Int ~> ConT ''String === Int -> String@
(~>) :: Type -> Type -> Type
a ~> b = ArrowT `AppT` a `AppT` b
infixr 1 ~>

-- TODO: process isRoot
compileSymbolTable :: SymbolTable EnumDecl StructDecl TableDecl UnionDecl -> Q [Dec]
compileSymbolTable symbols = do
  enumDecs <- join <$> traverse mkEnum (allEnums symbols)
  structDecs <- join <$> traverse mkStruct (allStructs symbols)
  tableDecs <- join <$> traverse mkTable (allTables symbols)
  unionDecs <- join <$> traverse mkUnion (allUnions symbols)
  pure $ enumDecs <> structDecs <> tableDecs <> unionDecs

mkEnum :: (Namespace, EnumDecl) -> Q [Dec]
mkEnum (_, enum) = do
  enumName <- newNameFor NC.dataTypeName enum

  enumValNames <-
    forM (enumVals enum) $ \enumVal ->
      newName $ T.unpack $ NC.enumUnionMember enum enumVal

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
  let funName = mkName $ "to" <> T.unpack (NC.typ (unIdent (getIdent enum)))
  argName <- newName "n"
  pure
    [ SigD funName (enumTypeToType (enumType enum) ~> ConT ''Maybe `AppT` ConT enumName)
    , FunD funName
      [ Clause
        [VarP argName]
        (NormalB (CaseE (VarE argName) matches))
        []
      ]
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
  let funName = mkName $ "from" <> T.unpack (NC.typ (unIdent (getIdent enum)))
  argName <- newName "n"
  pure
    [ SigD funName (ConT enumName ~> enumTypeToType (enumType enum))
    , FunD funName
      [ Clause
        [VarP argName]
        (NormalB (CaseE (VarE argName) (mkMatch <$> NE.toList enumValsAndNames)))
        []
      ]
    ]
  where
    mkMatch (enumVal, enumName) =
      Match
        (ConP enumName [])
        (NormalB (intLitE (enumValInt enumVal)))
        []


mkStruct :: (Namespace, StructDecl) -> Q [Dec]
mkStruct (_, struct) = do
  structName <- newNameFor NC.dataTypeName struct
  let dataDec = DataD [] structName [] Nothing [] []
  (consSig, cons) <- mkStructConstructor structName struct

  let getters = foldMap (mkStructFieldGetter structName struct) (structFields struct)

  pure $
    [ dataDec
    , consSig
    , cons
    ] <> getters


mkStructConstructor :: Name -> StructDecl -> Q (Dec, Dec)
mkStructConstructor structName struct = do
  argsInfo <- traverse mkStructConstructorArg (structFields struct)
  let (argTypes, pats, exps) = nonEmptyUnzip3 argsInfo

  let retType = AppT (ConT ''WriteStruct) (ConT structName)
  let sigType = foldr (~>) retType argTypes

  let consName = mkNameFor NC.dataTypeConstructor struct
  let consSig = SigD consName sigType

  let body = NormalB $ app
        [ VarE 'writeStruct
        , intLitE (structAlignment struct)
        , nonEmptyE (NE.reverse exps)
        ]

  let cons = FunD consName [ Clause (NE.toList pats) body [] ]

  pure (consSig, cons)


mkStructConstructorArg :: StructField -> Q (Type, Pat, Exp)
mkStructConstructorArg sf = do
  argName <- newNameFor NC.term sf
  let argPat = VarP argName
  let argRef = VarE argName
  let argType = structFieldTypeToWriteType (structFieldType sf)

  let mkWriteExp sft =
        case sft of
          SInt8            -> VarE 'int8
          SInt16           -> VarE 'int16
          SInt32           -> VarE 'int32
          SInt64           -> VarE 'int64
          SWord8           -> VarE 'word8
          SWord16          -> VarE 'word16
          SWord32          -> VarE 'word32
          SWord64          -> VarE 'word64
          SFloat           -> VarE 'float
          SDouble          -> VarE 'double
          SBool            -> VarE 'bool
          SEnum _ enumType -> mkWriteExp (enumTypeToStructFieldType enumType)
          SStruct _        -> VarE 'unWriteStruct

  let expWithoutPadding = mkWriteExp (structFieldType sf) `AppE` argRef

  let exp =
        if structFieldPadding sf == 0
          then expWithoutPadding
          else app
            [ VarE 'padded
            , intLitE (structFieldPadding sf)
            , expWithoutPadding
            ]

  pure (argType, argPat, exp)

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
            ForallT [PlainTV (mkName "m")] [ConT ''ReadCtx `AppT` VarT (mkName "m")] $
              ConT ''Struct `AppT` ConT structName ~> VarT (mkName "m") `AppT` retType

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
        SStruct (namespace, structDecl) -> VarE 'readStruct

mkTable :: (Namespace, TableDecl) -> Q [Dec]
mkTable (_, table) = do
  tableName <- newNameFor NC.dataTypeName table
  (consSig, cons) <- mkTableConstructor tableName table

  let getters = foldMap (mkTableFieldGetter tableName table) (tableFields table)

  pure $
    [ DataD [] tableName [] Nothing [] []
    , consSig
    , cons
    ] <> getters

mkTableConstructor :: Name -> TableDecl -> Q (Dec, Dec)
mkTableConstructor tableName table = do
  (argTypes, pats, exps, whereBindings) <- mconcat <$> traverse mkTableContructorArg (tableFields table)

  let retType = AppT (ConT ''WriteTable) (ConT tableName)
  let sigType = foldr (~>) retType argTypes

  let consName = mkNameFor NC.dataTypeConstructor table
  let consSig = SigD consName sigType

  let body = NormalB $ AppE (VarE 'writeTable) (ListE exps)
  let cons = FunD consName [ Clause pats body whereBindings ]

  pure (consSig, cons)

mkTableContructorArg :: TableField -> Q ([Type], [Pat], [Exp], [Dec])
mkTableContructorArg tf =
  if tableFieldDeprecated tf
    then
      case tableFieldType tf of
        TUnion _ _           -> pure ([], [], [VarE 'deprecated, VarE 'deprecated], [])
        TVector _ (VUnion _) -> pure ([], [], [VarE 'deprecated, VarE 'deprecated], [])
        _                    -> pure ([], [], [VarE 'deprecated], [])
    else do
      argName <- newNameFor NC.term tf
      let argPat = VarP argName
      let argRef = VarE argName
      let argType = tableFieldTypeToWriteType (tableFieldType tf)
      let exps = mkExps argRef (tableFieldType tf)

      pure ([argType], [argPat], exps, [])

  where
    expForScalar :: Exp -> Exp -> Exp -> Exp
    expForScalar defaultValExp writeExp varExp =
      VarE 'optionalDef `AppE` defaultValExp `AppE` (VarE 'inline `AppE` writeExp) `AppE` varExp

    mkExps :: Exp -> TableFieldType -> [Exp]
    mkExps argRef tfType =
        case tfType of
          TInt8   (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'int8   ) argRef ]
          TInt16  (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'int16  ) argRef ]
          TInt32  (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'int32  ) argRef ]
          TInt64  (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'int64  ) argRef ]
          TWord8  (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'word8  ) argRef ]
          TWord16 (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'word16 ) argRef ]
          TWord32 (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'word32 ) argRef ]
          TWord64 (DefaultVal n) -> [ expForScalar (intLitE n)  (VarE 'word64 ) argRef ]
          TFloat  (DefaultVal n) -> [ expForScalar (realLitE n) (VarE 'float  ) argRef ]
          TDouble (DefaultVal n) -> [ expForScalar (realLitE n) (VarE 'double ) argRef ]
          TBool   (DefaultVal b) -> [ expForScalar (if b then ConE 'True else ConE 'False)  (VarE 'bool   ) argRef ]
          TString req            -> [ requiredExp req (VarE 'text) `AppE` argRef ]
          TEnum _ enumType dflt  -> mkExps argRef (enumTypeToTableFieldType enumType dflt)
          TStruct _ req          -> [ requiredExp req (VarE 'inline `AppE` VarE 'unWriteStruct) `AppE` argRef ]
          TTable _ req           -> [ requiredExp req (VarE 'unWriteTable) `AppE` argRef ]
          TUnion _ _             -> [ VarE 'writeUnionType `AppE` argRef
                                    , VarE 'writeUnionValue `AppE` argRef
                                    ]
          TVector req vecElemType -> mkExpForVector argRef req vecElemType

    mkExpForVector :: Exp -> Required -> VectorElementType -> [Exp]
    mkExpForVector argRef req vecElemType =
        case vecElemType of
          VInt8   -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'int8   )) `AppE` argRef ]
          VInt16  -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'int16  )) `AppE` argRef ]
          VInt32  -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'int32  )) `AppE` argRef ]
          VInt64  -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'int64  )) `AppE` argRef ]
          VWord8  -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'word8  )) `AppE` argRef ]
          VWord16 -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'word16 )) `AppE` argRef ]
          VWord32 -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'word32 )) `AppE` argRef ]
          VWord64 -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'word64 )) `AppE` argRef ]
          VFloat  -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'float  )) `AppE` argRef ]
          VDouble -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'double )) `AppE` argRef ]
          VBool   -> [ requiredExp req (VarE 'writeVector `AppE` (VarE 'inline `AppE` VarE 'bool   )) `AppE` argRef ]
          VString -> [ requiredExp req (VarE 'writeVector `AppE`                      VarE 'text    ) `AppE` argRef ]
          VEnum _ enumType _ -> mkExpForVector argRef req (enumTypeToVectorElementType enumType)
          _ -> undefined

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
        ForallT [PlainTV (mkName "m")] [ConT ''ReadCtx `AppT` VarT (mkName "m")] $
          ConT ''Table `AppT` ConT tableName ~> VarT (mkName "m") `AppT` tableFieldTypeToReadType (tableFieldType tf)

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
        TStruct _ req           -> mkFunWithBody (bodyForNonScalar req (VarE 'readStruct'))
        TTable _ req            -> mkFunWithBody (bodyForNonScalar req (VarE 'readTable))
        TUnion (TypeRef ns ident) req ->
          mkFunWithBody $ app
            [ VarE 'readTableFieldUnion
            , VarE . mkName . T.unpack . NC.withModulePrefix ns $ NC.unionReadFun ident
            , fieldIndex
            ]
        TVector req vecElemType -> mkFunForVector req vecElemType

    mkFunForVector :: Required -> VectorElementType -> Dec
    mkFunForVector req vecElemType =
      case vecElemType of
        VInt8   -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Int8Vec
        VInt16  -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Int16Vec
        VInt32  -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Int32Vec
        VInt64  -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Int64Vec
        VWord8  -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Word8Vec
        VWord16 -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Word16Vec
        VWord32 -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Word32Vec
        VWord64 -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'Word64Vec
        VFloat  -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'FloatVec
        VDouble -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'DoubleVec
        VBool   -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'BoolVec
        VString -> mkFunWithBody $ bodyForNonScalar req $ VarE 'readPrimVector `AppE` ConE 'TextVec
        VEnum _ enumType _ -> mkFunForVector req (enumTypeToVectorElementType enumType)
        _ -> undefined

    mkFunWithBody :: Exp -> Dec
    mkFunWithBody body = FunD funName [ Clause [] (NormalB body) [] ]

    bodyForNonScalar req readExp =
      case req of
        Req ->
          app
            [ VarE 'readTableFieldReq
            , readExp
            , fieldIndex
            , LitE (StringL (T.unpack . unIdent . getIdent $ tf))
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
  unionName <- newNameFor NC.dataTypeName union
  unionValNames <-
    forM (unionVals union) $ \unionVal ->
      newName $ T.unpack $ NC.enumUnionMember union unionVal

  unionClassDecs <- mkUnionClass unionName union

  readFun <- mkUnionReadFun unionName unionValNames union

  pure $
    mkUnionDataDec unionName (unionVals union `NE.zip` unionValNames)
    : unionClassDecs
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

mkUnionClass :: Name -> UnionDecl -> Q [Dec]
mkUnionClass unionName union = do
  className <- newName $ T.unpack $ NC.unionClass union
  methodName <- newNameFor NC.term union
  let cls =
        ClassD [] className
          [PlainTV (mkName "a")]
          []
          [ SigD methodName $
              ConT ''WriteTable `AppT` VarT (mkName "a") ~> ConT ''WriteUnion `AppT` ConT unionName
          ]
  let
    mkUnionInstance :: UnionVal -> Integer -> Dec
    mkUnionInstance unionVal ix =
      InstanceD
        Nothing
        []
        (ConT className `AppT` typeRefToType (unionValTableRef unionVal))
        [ FunD methodName
            [ Clause
                []
                (NormalB $ VarE 'writeUnion `AppE` intLitE ix)
                []
            ]
        ]
  let instances = uncurry mkUnionInstance <$> NE.toList (unionVals union) `zip` [1..]

  pure $ cls : instances

mkUnionReadFun :: Name -> NonEmpty Name -> UnionDecl -> Q [Dec]
mkUnionReadFun unionName unionValNames union = do
  nArg <- newName "n"
  posArg <- newName "pos"
  wildcard <- newName "n'"

  let funName = mkName $ T.unpack $ NC.unionReadFun union
  let sig =
        SigD funName $
          ForallT [PlainTV (mkName "m")] [ConT ''ReadCtx `AppT` VarT (mkName "m")] $
            ConT ''Positive `AppT` ConT ''Word8
              ~> ConT ''PositionInfo
              ~> VarT (mkName "m") `AppT` (ConT ''Union `AppT` ConT unionName)

  let
    mkMatch :: Name -> Integer -> Match
    mkMatch unionValName ix =
      Match
        (intLitP ix)
        (NormalB $
          InfixE
            (Just (compose [ConE 'Union, ConE unionValName]))
            (VarE '(<$>))
            (Just (VarE 'readTable `AppE` VarE posArg))
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
        FunD funName $
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
    VEnum   _ enumType _  -> ConT ''WriteVector `AppT` enumTypeToType enumType
    VStruct typeRef _     -> ConT ''WriteVector `AppT` (ConT ''WriteStruct `AppT` typeRefToType typeRef)
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
    VEnum   _ enumType _  -> ConT ''Vector `AppT` enumTypeToType enumType
    VStruct typeRef _     -> ConT ''Vector `AppT` (ConT ''Struct `AppT` typeRefToType typeRef)
    VTable  typeRef       -> ConT ''Vector `AppT` (ConT ''Table  `AppT` typeRefToType typeRef)
    VUnion  typeRef       -> ConT ''Vector `AppT` (ConT ''Union  `AppT` typeRefToType typeRef)

typeRefToType :: TypeRef -> Type
typeRefToType (TypeRef ns ident) =
  ConT (mkNameFor (NC.withModulePrefix ns . NC.dataTypeName) ident)

requiredExp :: Required -> Exp -> Exp
requiredExp Req exp = exp
requiredExp Opt exp = AppE (VarE 'optional) exp

requiredType :: Required -> Type -> Type
requiredType Req t = t
requiredType Opt t = AppT (ConT ''Maybe) t

mkName' :: HasIdent a => a -> Name
mkName' = mkName . T.unpack . unIdent . getIdent

newName' :: HasIdent a => a -> Q Name
newName' = newName . T.unpack . unIdent . getIdent

mkNameFor :: HasIdent a => (Text -> Text) -> a -> Name
mkNameFor f = mkName . T.unpack . f . unIdent . getIdent

newNameFor :: HasIdent a => (Text -> Text) -> a -> Q Name
newNameFor f = newName . T.unpack . f . unIdent . getIdent


intLitP :: Integral i => i -> Pat
intLitP = LitP . IntegerL . toInteger

intLitE :: Integral i => i -> Exp
intLitE = LitE . IntegerL . toInteger

realLitE :: Real i => i -> Exp
realLitE = LitE . RationalL . toRational

nonEmptyE :: NonEmpty Exp -> Exp
nonEmptyE (x :| xs) = InfixE (Just x) (ConE '(:|)) (Just (ListE xs))

-- | Applies a function to multiple arguments. Assumes the list is not empty.
app :: [Exp] -> Exp
app = foldl1 AppE

compose :: [Exp] -> Exp
compose = foldr1 (\e1 e2 -> InfixE (Just e1) (VarE '(.)) (Just e2))
