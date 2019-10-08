{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FlatBuffers.Internal.Compiler.SemanticAnalysis where

import           Control.Monad                                 ( forM_, join, when )
import           Control.Monad.Except                          ( MonadError, throwError )
import           Control.Monad.Reader                          ( MonadReader(..), asks, runReaderT )
import           Control.Monad.State                           ( MonadState, State, StateT, evalState, evalStateT, get, modify, put )

import           Data.Bits                                     ( (.&.), (.|.), Bits, FiniteBits, bit, finiteBitSize )
import           Data.Coerce                                   ( coerce )
import           Data.Foldable                                 ( asum, find, foldlM, traverse_ )
import qualified Data.Foldable                                 as Foldable
import           Data.Functor                                  ( ($>), (<&>) )
import           Data.Int
import           Data.Ix                                       ( inRange )
import qualified Data.List                                     as List
import           Data.List.NonEmpty                            ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty                            as NE
import           Data.Map.Strict                               ( Map )
import qualified Data.Map.Strict                               as Map
import           Data.Maybe                                    ( catMaybes, fromMaybe, isJust )
import           Data.Monoid                                   ( Sum(..) )
import           Data.Scientific                               ( Scientific )
import qualified Data.Scientific                               as Scientific
import           Data.Set                                      ( Set )
import qualified Data.Set                                      as Set
import           Data.Text                                     ( Text )
import qualified Data.Text                                     as T
import           Data.Traversable                              ( for )
import           Data.Word

import           FlatBuffers.Internal.Compiler.Display         ( Display(..) )
import           FlatBuffers.Internal.Compiler.SyntaxTree      ( FileTree(..), HasIdent(..), HasMetadata(..), Ident, Namespace, Schema, TypeRef(..), qualify )
import qualified FlatBuffers.Internal.Compiler.SyntaxTree      as ST
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree
import           FlatBuffers.Internal.Constants
import           FlatBuffers.Internal.Types

import           Text.Read                                     ( readMaybe )


type ValidationCtx m = (MonadError String m, MonadReader ValidationState m)

data ValidationState = ValidationState
  { validationStateCurrentContext :: !Ident
    -- ^ The thing being validated (e.g. a fully-qualified struct name, or a table field name).
  , validationStateAllAttributes  :: !(Set ST.AttributeDecl)
    -- ^ All the attributes declared in all the schemas (including imported ones).
  }


modifyContext :: ValidationCtx m => (Ident -> Ident) -> m a -> m a
modifyContext f =
  local $ \s ->
    s { validationStateCurrentContext = f (validationStateCurrentContext s) }

data SymbolTable enum struct table union = SymbolTable
  { allEnums   :: ![(Namespace, enum)]
  , allStructs :: ![(Namespace, struct)]
  , allTables  :: ![(Namespace, table)]
  , allUnions  :: ![(Namespace, union)]
  }
  deriving (Eq, Show)

instance Semigroup (SymbolTable e s t u)  where
  SymbolTable e1 s1 t1 u1 <> SymbolTable e2 s2 t2 u2 =
    SymbolTable (e1 <> e2) (s1 <> s2) (t1 <> t2) (u1 <> u2)

instance Monoid (SymbolTable e s t u) where
  mempty = SymbolTable [] [] [] []

type Stage1     = SymbolTable ST.EnumDecl ST.StructDecl ST.TableDecl ST.UnionDecl
type Stage2     = SymbolTable    EnumDecl ST.StructDecl ST.TableDecl ST.UnionDecl
type Stage3     = SymbolTable    EnumDecl    StructDecl ST.TableDecl ST.UnionDecl
type Stage4     = SymbolTable    EnumDecl    StructDecl    TableDecl ST.UnionDecl
type ValidDecls = SymbolTable    EnumDecl    StructDecl    TableDecl    UnionDecl

-- | Takes a collection of schemas, and pairs each type declaration with its corresponding namespace
createSymbolTables :: FileTree Schema -> FileTree Stage1
createSymbolTables = fmap (pairDeclsWithNamespaces . ST.decls)
  where
    pairDeclsWithNamespaces :: [ST.Decl] -> Stage1
    pairDeclsWithNamespaces = snd . foldl go ("", mempty)

    go :: (Namespace, Stage1) -> ST.Decl -> (Namespace, Stage1)
    go (currentNamespace, decls) decl =
      case decl of
        ST.DeclN (ST.NamespaceDecl newNamespace) -> (newNamespace, decls)
        ST.DeclE enum   -> (currentNamespace, decls <> SymbolTable [(currentNamespace, enum)] [] [] [])
        ST.DeclS struct -> (currentNamespace, decls <> SymbolTable [] [(currentNamespace, struct)] [] [])
        ST.DeclT table  -> (currentNamespace, decls <> SymbolTable [] [] [(currentNamespace, table)] [])
        ST.DeclU union  -> (currentNamespace, decls <> SymbolTable [] [] [] [(currentNamespace, union)])
        _               -> (currentNamespace, decls)

validateSchemas :: MonadError String m => FileTree Schema -> m (FileTree ValidDecls)
validateSchemas schemas =
  flip runReaderT (ValidationState "" allAttributes) $ do
    checkDuplicateIdentifiers allQualifiedTopLevelIdentifiers
    validateEnums symbolTables
      >>= validateStructs
      >>= validateTables
      >>= validateUnions
      >>= updateRootTable (fileTreeRoot schemas)
  where
    symbolTables = createSymbolTables schemas

    allQualifiedTopLevelIdentifiers =
      flip concatMap symbolTables $ \symbolTable ->
        join
          [ uncurry qualify <$> allEnums symbolTable
          , uncurry qualify <$> allStructs symbolTable
          , uncurry qualify <$> allTables symbolTable
          , uncurry qualify <$> allUnions symbolTable
          ]

    declaredAttributes =
      flip concatMap schemas $ \schema ->
        [ attr | ST.DeclA attr <- ST.decls schema ]

    allAttributes = Set.fromList $ declaredAttributes <> knownAttributes

----------------------------------
------------ Root Type -----------
----------------------------------
data RootInfo = RootInfo
  { rootTableNamespace :: !Namespace
  , rootTable          :: !TableDecl
  , rootFileIdent      :: !(Maybe Text)
  }

-- | Finds the root table (if any) and sets the `tableIsRoot` flag accordingly.
-- We only care about @root_type@ declarations in the root schema. Imported schemas are not scanned for @root_type@s.
-- The root type declaration can point to a table in any schema (root or imported).
updateRootTable :: forall m. ValidationCtx m => Schema -> FileTree ValidDecls -> m (FileTree ValidDecls)
updateRootTable schema symbolTables =
  getRootInfo schema symbolTables <&> \case
    Just rootInfo -> updateSymbolTable rootInfo <$> symbolTables
    Nothing       -> symbolTables

  where
    updateSymbolTable :: RootInfo -> ValidDecls -> ValidDecls
    updateSymbolTable rootInfo st = st { allTables = updateTable rootInfo <$> allTables st}

    updateTable :: RootInfo -> (Namespace, TableDecl) -> (Namespace, TableDecl)
    updateTable (RootInfo rootTableNamespace rootTable fileIdent) pair@(namespace, table) =
      if namespace == rootTableNamespace && table == rootTable
        then (namespace, table { tableIsRoot = IsRoot fileIdent })
        else pair

getRootInfo :: forall m. ValidationCtx m => Schema -> FileTree ValidDecls -> m (Maybe RootInfo)
getRootInfo schema symbolTables =
  foldlM go ("", Nothing, Nothing) (ST.decls schema) <&> \case
    (_, Just (rootTableNamespace, rootTable), fileIdent) -> Just $ RootInfo rootTableNamespace rootTable fileIdent
    _ -> Nothing
  where
    go :: (Namespace, Maybe (Namespace, TableDecl), Maybe Text) -> ST.Decl -> m (Namespace, Maybe (Namespace, TableDecl), Maybe Text)
    go state@(currentNamespace, rootInfo, fileIdent) decl =
      case decl of
        ST.DeclN (ST.NamespaceDecl newNamespace)       -> pure (newNamespace, rootInfo, fileIdent)
        ST.DeclFI (ST.FileIdentifierDecl newFileIdent) -> pure (currentNamespace, rootInfo, Just (coerce newFileIdent))
        ST.DeclR (ST.RootDecl typeRef)                 ->
          findDecl currentNamespace symbolTables typeRef >>= \case
            MatchT (rootTableNamespace, rootTable)  -> pure (currentNamespace, Just (rootTableNamespace, rootTable), fileIdent)
            _                                       -> throwErrorMsg "root type must be a table"
        _ -> pure state


----------------------------------
----------- Attributes -----------
----------------------------------
knownAttributes :: [ST.AttributeDecl]
knownAttributes =
  coerce
    [ idAttr
    , deprecatedAttr
    , requiredAttr
    , forceAlignAttr
    , bitFlagsAttr
    ]
  <> otherKnownAttributes

idAttr, deprecatedAttr, requiredAttr, forceAlignAttr, bitFlagsAttr :: Text
idAttr          = "id"
deprecatedAttr  = "deprecated"
requiredAttr    = "required"
forceAlignAttr  = "force_align"
bitFlagsAttr    = "bit_flags"

otherKnownAttributes :: [ST.AttributeDecl]
otherKnownAttributes =
  -- https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html
  [ "nested_flatbuffer"
  , "flexbuffer"
  , "key"
  , "hash"
  , "original_order"
  -- https://google.github.io/flatbuffers/flatbuffers_guide_use_cpp.html#flatbuffers_cpp_object_based_api
  , "native_inline"
  , "native_default"
  , "native_custom_alloc"
  , "native_type"
  , "cpp_type"
  , "cpp_ptr_type"
  , "cpp_str_type"
  , "cpp_str_flex_ctor"
  , "shared"
  ]

----------------------------------
--------- Symbol search ----------
----------------------------------
data Match enum struct table union
  = MatchE !(Namespace, enum)
  | MatchS !(Namespace, struct)
  | MatchT !(Namespace, table)
  | MatchU !(Namespace, union)

-- | Looks for a type reference in a set of type declarations.
-- If none is found, the list of namespaces in which the type reference was searched for is returned.
findDecl ::
     ValidationCtx m
  => (HasIdent e, HasIdent s, HasIdent t, HasIdent u)
  => Namespace
  -> FileTree (SymbolTable e s t u)
  -> TypeRef
  -> m (Match e s t u)
findDecl currentNamespace symbolTables typeRef@(TypeRef refNamespace refIdent) =
  let parentNamespaces' = parentNamespaces currentNamespace
      results = do
        parentNamespace <- parentNamespaces'
        let candidateNamespace = parentNamespace <> refNamespace
        let searchSymbolTable symbolTable =
              asum
                [ MatchE <$> find (\(ns, e) -> ns == candidateNamespace && getIdent e == refIdent) (allEnums symbolTable)
                , MatchS <$> find (\(ns, e) -> ns == candidateNamespace && getIdent e == refIdent) (allStructs symbolTable)
                , MatchT <$> find (\(ns, e) -> ns == candidateNamespace && getIdent e == refIdent) (allTables symbolTable)
                , MatchU <$> find (\(ns, e) -> ns == candidateNamespace && getIdent e == refIdent) (allUnions symbolTable)
                ]
        pure $ asum $ fmap searchSymbolTable symbolTables
  in
    case asum results of
      Just match -> pure match
      Nothing    ->
        throwErrorMsg $
          "type '"
          <> display typeRef
          <> "' does not exist (checked in these namespaces: "
          <> display parentNamespaces'
          <> ")"

-- | Returns a list of all the namespaces "between" the current namespace
-- and the root namespace, in that order.
-- See: https://github.com/google/flatbuffers/issues/5234#issuecomment-471680403
--
-- > parentNamespaces "A.B.C" == ["A.B.C", "A.B", "A", ""]
parentNamespaces :: ST.Namespace -> NonEmpty ST.Namespace
parentNamespaces (ST.Namespace ns) =
  coerce $ NE.reverse $ NE.inits ns

----------------------------------
------------- Enums --------------
----------------------------------
validateEnums :: forall m. ValidationCtx m => FileTree Stage1 -> m (FileTree Stage2)
validateEnums symbolTables =
  for symbolTables $ \symbolTable -> do
    let enums = allEnums symbolTable
    let validate (namespace, enum) = do
          validEnum <- validateEnum (namespace, enum)
          pure (namespace, validEnum)
    validEnums <- traverse validate enums
    pure symbolTable { allEnums = validEnums }

validateEnum :: forall m. ValidationCtx m => (Namespace, ST.EnumDecl) -> m EnumDecl
validateEnum (currentNamespace, enum) =
  modifyContext (\_ -> qualify currentNamespace enum) $ do
    checkDuplicateFields
    checkUndeclaredAttributes enum
    validEnum
  where
    isBitFlags = hasAttribute bitFlagsAttr (ST.enumMetadata enum)

    validEnum = do
      enumType <- validateEnumType (ST.enumType enum)
      let enumVals = flip evalState Nothing . traverse mapEnumVal $ ST.enumVals enum
      validateOrder enumVals
      traverse_ (validateBounds enumType) enumVals
      pure EnumDecl
        { enumIdent = getIdent enum
        , enumType = enumType
        , enumBitFlags = isBitFlags
        , enumVals = shiftBitFlags <$> enumVals
        }

    mapEnumVal :: ST.EnumVal -> State (Maybe Integer) EnumVal
    mapEnumVal enumVal = do
      thisInt <-
        case ST.enumValLiteral enumVal of
          Just (ST.IntLiteral thisInt) ->
            pure thisInt
          Nothing ->
            get <&> \case
              Just lastInt -> lastInt + 1
              Nothing      -> 0
      put (Just thisInt)
      pure (EnumVal (getIdent enumVal) thisInt)

    validateOrder :: NonEmpty EnumVal -> m ()
    validateOrder xs =
      if all (\(x, y) -> enumValInt x < enumValInt y) (NE.toList xs `zip` NE.tail xs)
        then pure ()
        else throwErrorMsg "enum values must be specified in ascending order"

    validateBounds :: EnumType -> EnumVal -> m ()
    validateBounds enumType enumVal =
      modifyContext (\context -> context <> "." <> getIdent enumVal) $
        case enumType of
          EInt8 -> validateBounds' @Int8 enumVal
          EInt16 -> validateBounds' @Int16 enumVal
          EInt32 -> validateBounds' @Int32 enumVal
          EInt64 -> validateBounds' @Int64 enumVal
          EWord8 -> validateBounds' @Word8 enumVal
          EWord16 -> validateBounds' @Word16 enumVal
          EWord32 -> validateBounds' @Word32 enumVal
          EWord64 -> validateBounds' @Word64 enumVal

    validateBounds' :: forall a. (FiniteBits a, Integral a, Bounded a) => EnumVal -> m ()
    validateBounds' e =
      if inRange (lower, upper) (enumValInt e)
        then pure ()
        else throwErrorMsg $
              "enum value of "
              <> display (enumValInt e)
              <> " does not fit ["
              <> display lower
              <> "; "
              <> display upper
              <> "]"
      where
        lower = if isBitFlags
                  then 0
                  else toInteger (minBound @a)
        upper = if isBitFlags
                  then toInteger (finiteBitSize @a (undefined :: a) - 1)
                  else toInteger (maxBound @a)

    validateEnumType :: ST.Type -> m EnumType
    validateEnumType t =
      case t of
        ST.TInt8   -> unlessIsBitFlags EInt8
        ST.TInt16  -> unlessIsBitFlags EInt16
        ST.TInt32  -> unlessIsBitFlags EInt32
        ST.TInt64  -> unlessIsBitFlags EInt64
        ST.TWord8  -> pure EWord8
        ST.TWord16 -> pure EWord16
        ST.TWord32 -> pure EWord32
        ST.TWord64 -> pure EWord64
        _          -> throwErrorMsg "underlying enum type must be integral"
      where
        unlessIsBitFlags x =
          if isBitFlags
            then throwErrorMsg "underlying type of bit_flags enum must be unsigned"
            else pure x

    -- If this enum has the `bit_flags` attribute, convert its int value to the corresponding bitmask.
    -- E.g., 2 -> 00000100
    shiftBitFlags :: EnumVal -> EnumVal
    shiftBitFlags e =
      if isBitFlags
        then e { enumValInt = bit (fromIntegral @Integer @Int (enumValInt e)) }
        else e

    checkDuplicateFields :: m ()
    checkDuplicateFields =
      checkDuplicateIdentifiers
        (ST.enumVals enum)


----------------------------------
------------ Tables --------------
----------------------------------
data TableFieldWithoutId = TableFieldWithoutId !Ident !TableFieldType !Bool

validateTables :: ValidationCtx m => FileTree Stage3 -> m (FileTree Stage4)
validateTables symbolTables =
  for symbolTables $ \symbolTable -> do
    let tables = allTables symbolTable
    let validate (namespace, table) = do
          validTable <- validateTable symbolTables (namespace, table)
          pure (namespace, validTable)
    validTables <- traverse validate tables
    pure symbolTable { allTables = validTables }

validateTable :: forall m. ValidationCtx m => FileTree Stage3 -> (Namespace, ST.TableDecl) -> m TableDecl
validateTable symbolTables (currentNamespace, table) =
  modifyContext (\_ -> qualify currentNamespace table) $ do

    let fields = ST.tableFields table
    let fieldsMetadata = ST.tableFieldMetadata <$> fields

    checkDuplicateFields fields
    checkUndeclaredAttributes table

    validFieldsWithoutIds <- traverse validateTableField fields
    validFields <- assignFieldIds fieldsMetadata validFieldsWithoutIds

    pure TableDecl
      { tableIdent = getIdent table
      , tableIsRoot = NotRoot
      , tableFields = validFields
      }

  where
    checkDuplicateFields :: [ST.TableField] -> m ()
    checkDuplicateFields = checkDuplicateIdentifiers

    assignFieldIds :: [ST.Metadata] -> [TableFieldWithoutId] -> m [TableField]
    assignFieldIds metadata fieldsWithoutIds = do
      ids <- catMaybes <$> traverse (findIntAttr idAttr) metadata
      if null ids
        then pure $ evalState (traverse assignFieldId fieldsWithoutIds) (-1)
        else if length ids == length fieldsWithoutIds
          then do
            let fields = zipWith (\(TableFieldWithoutId ident typ depr) id -> TableField id ident typ depr) fieldsWithoutIds ids
            let sorted = List.sortOn tableFieldId fields
            evalStateT (traverse_ checkFieldId sorted) (-1)
            pure sorted
          else
            throwErrorMsg "either all fields or no fields must have an 'id' attribute"

    assignFieldId :: TableFieldWithoutId -> State Integer TableField
    assignFieldId (TableFieldWithoutId ident typ depr) = do
      lastId <- get
      let fieldId =
            case typ of
              TUnion _ _           -> lastId + 2
              TVector _ (VUnion _) -> lastId + 2
              _                    -> lastId + 1
      put fieldId
      pure (TableField fieldId ident typ depr)

    checkFieldId :: TableField -> StateT Integer m ()
    checkFieldId field = do
      lastId <- get
      modifyContext (\context -> context <> "." <> getIdent field) $ do
        case tableFieldType field of
          TUnion _ _ ->
            when (tableFieldId field /= lastId + 2) $
              throwErrorMsg "the id of a union field must be the last field's id + 2"
          TVector _ (VUnion _) ->
            when (tableFieldId field /= lastId + 2) $
              throwErrorMsg "the id of a vector of unions field must be the last field's id + 2"
          _ ->
            when (tableFieldId field /= lastId + 1) $
              throwErrorMsg $ "field ids must be consecutive from 0; id " <> display (lastId + 1) <> " is missing"
        put (tableFieldId field)

    validateTableField :: ST.TableField -> m TableFieldWithoutId
    validateTableField tf =
      modifyContext (\context -> context <> "." <> getIdent tf) $ do
        checkUndeclaredAttributes tf
        validFieldType <- validateTableFieldType (ST.tableFieldMetadata tf) (ST.tableFieldDefault tf) (ST.tableFieldType tf)

        pure $ TableFieldWithoutId
          (getIdent tf)
          validFieldType
          (hasAttribute deprecatedAttr (ST.tableFieldMetadata tf))

    validateTableFieldType :: ST.Metadata -> Maybe ST.DefaultVal -> ST.Type -> m TableFieldType
    validateTableFieldType md dflt tableFieldType =
      case tableFieldType of
        ST.TInt8 -> checkNoRequired md >> validateDefaultValAsInt @Int8 dflt <&> TInt8
        ST.TInt16 -> checkNoRequired md >> validateDefaultValAsInt @Int16 dflt <&> TInt16
        ST.TInt32 -> checkNoRequired md >> validateDefaultValAsInt @Int32 dflt <&> TInt32
        ST.TInt64 -> checkNoRequired md >> validateDefaultValAsInt @Int64 dflt <&> TInt64
        ST.TWord8 -> checkNoRequired md >> validateDefaultValAsInt @Word8 dflt <&> TWord8
        ST.TWord16 -> checkNoRequired md >> validateDefaultValAsInt @Word16 dflt <&> TWord16
        ST.TWord32 -> checkNoRequired md >> validateDefaultValAsInt @Word32 dflt <&> TWord32
        ST.TWord64 -> checkNoRequired md >> validateDefaultValAsInt @Word64 dflt <&> TWord64
        ST.TFloat -> checkNoRequired md >> validateDefaultValAsScientific dflt <&> TFloat
        ST.TDouble -> checkNoRequired md >> validateDefaultValAsScientific dflt <&> TDouble
        ST.TBool -> checkNoRequired md >> validateDefaultValAsBool dflt <&> TBool
        ST.TString -> checkNoDefault dflt $> TString (isRequired md)
        ST.TRef typeRef ->
          findDecl currentNamespace symbolTables typeRef >>= \case
            MatchE (ns, enum) -> do
              checkNoRequired md
              validDefault <- validateDefaultAsEnum dflt enum
              pure $ TEnum (TypeRef ns (getIdent enum)) (enumType enum) validDefault
            MatchS (ns, struct) -> checkNoDefault dflt $> TStruct (TypeRef ns (getIdent struct))  (isRequired md)
            MatchT (ns, table)  -> checkNoDefault dflt $> TTable  (TypeRef ns (getIdent table)) (isRequired md)
            MatchU (ns, union)  -> checkNoDefault dflt $> TUnion  (TypeRef ns (getIdent union)) (isRequired md)
        ST.TVector vecType ->
          checkNoDefault dflt >> TVector (isRequired md) <$>
            case vecType of
              ST.TInt8 -> pure VInt8
              ST.TInt16 -> pure VInt16
              ST.TInt32 -> pure VInt32
              ST.TInt64 -> pure VInt64
              ST.TWord8 -> pure VWord8
              ST.TWord16 -> pure VWord16
              ST.TWord32 -> pure VWord32
              ST.TWord64 -> pure VWord64
              ST.TFloat -> pure VFloat
              ST.TDouble -> pure VDouble
              ST.TBool -> pure VBool
              ST.TString -> pure VString
              ST.TVector _ -> throwErrorMsg "nested vector types not supported"
              ST.TRef typeRef ->
                findDecl currentNamespace symbolTables typeRef <&> \case
                  MatchE (ns, enum) ->
                    VEnum (TypeRef ns (getIdent enum))
                          (enumType enum)
                  MatchS (ns, struct) ->
                    VStruct (TypeRef ns (getIdent struct))
                  MatchT (ns, table) -> VTable (TypeRef ns (getIdent table))
                  MatchU (ns, union) -> VUnion (TypeRef ns (getIdent union))

checkNoRequired :: ValidationCtx m => ST.Metadata -> m ()
checkNoRequired md =
  when (hasAttribute requiredAttr md) $
    throwErrorMsg "only non-scalar fields (strings, vectors, unions, structs, tables) may be 'required'"

checkNoDefault :: ValidationCtx m => Maybe ST.DefaultVal -> m ()
checkNoDefault dflt =
  when (isJust dflt) $
    throwErrorMsg
      "default values currently only supported for scalar fields (integers, floating point, bool, enums)"

isRequired :: ST.Metadata -> Required
isRequired md = if hasAttribute requiredAttr md then Req else Opt

validateDefaultValAsInt :: forall a m. (ValidationCtx m, Integral a, Bounded a, Display a) => Maybe ST.DefaultVal -> m (DefaultVal Integer)
validateDefaultValAsInt dflt =
  case dflt of
    Nothing                -> pure (DefaultVal 0)
    Just (ST.DefaultNum n) -> scientificToInteger @a n "default value must be integral"
    Just _                 -> throwErrorMsg "default value must be integral"

validateDefaultValAsScientific :: ValidationCtx m => Maybe ST.DefaultVal -> m (DefaultVal Scientific)
validateDefaultValAsScientific dflt =
  case dflt of
    Nothing                 -> pure (DefaultVal 0)
    Just (ST.DefaultNum n)  -> pure (DefaultVal n)
    Just _                  -> throwErrorMsg "default value must be a number"

validateDefaultValAsBool :: ValidationCtx m => Maybe ST.DefaultVal -> m (DefaultVal Bool)
validateDefaultValAsBool dflt =
  case dflt of
    Nothing                 -> pure (DefaultVal False)
    Just (ST.DefaultBool b) -> pure (DefaultVal b)
    Just _                  -> throwErrorMsg "default value must be a boolean"

validateDefaultAsEnum :: forall m. ValidationCtx m => Maybe ST.DefaultVal -> EnumDecl -> m (DefaultVal Integer)
validateDefaultAsEnum dflt enum =
  case dflt of
    Nothing ->
      if enumBitFlags enum
        then pure 0
        else
          case find (\val -> enumValInt val == 0) (enumVals enum) of
            Just _  -> pure 0
            Nothing -> throwErrorMsg "enum does not have a 0 value; please manually specify a default for this field"
    Just (ST.DefaultNum n) ->
      if enumBitFlags enum
        then
          case enumType enum of
            EWord8  -> scientificToInteger @Word8  n defaultErrorMsg
            EWord16 -> scientificToInteger @Word16 n defaultErrorMsg
            EWord32 -> scientificToInteger @Word32 n defaultErrorMsg
            EWord64 -> scientificToInteger @Word64 n defaultErrorMsg
            _       -> throwErrorMsg "The 'impossible' has happened: bit_flags enum with signed integer"
        else
          case Scientific.floatingOrInteger @Float n of
            Left _float -> throwErrorMsg defaultErrorMsg
            Right i ->
              case find (\val -> enumValInt val == i) (enumVals enum) of
                Just matchingVal -> pure (DefaultVal (enumValInt matchingVal))
                Nothing -> throwErrorMsg $ "default value of " <> display i <> " is not part of enum " <> display (getIdent enum)
    Just (ST.DefaultRef refs) ->
      if enumBitFlags enum
        then
          foldr1 (.|.) <$> traverse findEnumByRef refs
        else
          case refs of
            ref :| [] -> findEnumByRef ref
            _         -> throwErrorMsg $ "default value must be a single identifier, found " <> display (NE.length refs) <> ": " <> display refs
    Just (ST.DefaultBool _) ->
      throwErrorMsg defaultErrorMsg
  where
    defaultErrorMsg =
      if enumBitFlags enum
        then case enumVals enum of
          x :| y : _ ->
            "default value must be integral, one of ["
            <> display (getIdent <$> enumVals enum)
            <> "], or a combination of the latter in double quotes (e.g. \""
            <> display (getIdent x)
            <> " "
            <> display (getIdent y)
            <> "\")"
          _ ->
            "default value must be integral or one of: " <> display (getIdent <$> enumVals enum)
        else
          "default value must be integral or one of: " <> display (getIdent <$> enumVals enum)

    findEnumByRef :: Text -> m (DefaultVal Integer)
    findEnumByRef ref =
      case find (\val -> unIdent (getIdent val) == ref) (enumVals enum) of
        Just matchingVal -> pure (DefaultVal (enumValInt matchingVal))
        Nothing          -> throwErrorMsg $ "default value of " <> display ref <> " is not part of enum " <> display (getIdent enum)

scientificToInteger ::
  forall a m. (ValidationCtx m, Integral a, Bounded a, Display a)
  => Scientific -> String -> m (DefaultVal Integer)
scientificToInteger n notIntegerErrorMsg =
  if not (Scientific.isInteger n)
    then throwErrorMsg notIntegerErrorMsg
    else
      case Scientific.toBoundedInteger @a n of
        Nothing ->
          throwErrorMsg $
            "default value does not fit ["
            <> display (minBound @a)
            <> "; "
            <> display (maxBound @a)
            <> "]"
        Just i -> pure (DefaultVal (toInteger i))

----------------------------------
------------ Unions --------------
----------------------------------
validateUnions :: ValidationCtx m => FileTree Stage4 -> m (FileTree ValidDecls)
validateUnions symbolTables =
  for symbolTables $ \symbolTable -> do
    let unions = allUnions symbolTable
    let validate (namespace, union) = do
          validUnion <- validateUnion symbolTables (namespace, union)
          pure (namespace, validUnion)
    validUnions <- traverse validate unions
    pure symbolTable { allUnions = validUnions }

validateUnion :: forall m. ValidationCtx m => FileTree Stage4 -> (Namespace, ST.UnionDecl) -> m UnionDecl
validateUnion symbolTables (currentNamespace, union) =
  modifyContext (\_ -> qualify currentNamespace union) $ do
    validUnionVals <- traverse validateUnionVal (ST.unionVals union)
    checkDuplicateVals validUnionVals
    checkUndeclaredAttributes union
    pure $ UnionDecl
      { unionIdent = getIdent union
      , unionVals = validUnionVals
      }
  where
    validateUnionVal :: ST.UnionVal -> m UnionVal
    validateUnionVal uv = do
      let tref = ST.unionValTypeRef uv
      let partiallyQualifiedTypeRef = qualify (typeRefNamespace tref) (typeRefIdent tref)
      let ident = fromMaybe partiallyQualifiedTypeRef (ST.unionValIdent uv)
      let identFormatted = coerce $ T.replace "." "_" $ coerce ident
      modifyContext (\context -> context <> "." <> identFormatted) $ do
        tableRef <- validateUnionValType tref
        pure $ UnionVal
          { unionValIdent = identFormatted
          , unionValTableRef = tableRef
          }

    validateUnionValType :: TypeRef -> m TypeRef
    validateUnionValType typeRef =
      findDecl currentNamespace symbolTables typeRef >>= \case
        MatchT (ns, table)        -> pure $ TypeRef ns (getIdent table)
        _                         -> throwErrorMsg "union members may only be tables"

    checkDuplicateVals :: NonEmpty UnionVal -> m ()
    checkDuplicateVals vals = checkDuplicateIdentifiers (NE.cons "NONE" (fmap getIdent vals))


----------------------------------
------------ Structs -------------
----------------------------------
validateStructs :: ValidationCtx m => FileTree Stage2 -> m (FileTree Stage3)
validateStructs symbolTables =
  flip evalStateT [] $ traverse validateFile symbolTables
  where
  validateFile :: (MonadState [(Namespace, StructDecl)] m, ValidationCtx m) => Stage2 -> m Stage3
  validateFile symbolTable = do
    let structs = allStructs symbolTable

    traverse_ (checkStructCycles symbolTables) structs
    validStructs <- traverse (validateStruct symbolTables) structs

    pure symbolTable { allStructs = validStructs }

checkStructCycles :: forall m. ValidationCtx m => FileTree Stage2 -> (Namespace, ST.StructDecl) -> m ()
checkStructCycles symbolTables = go []
  where
    go :: [Ident] -> (Namespace, ST.StructDecl) -> m ()
    go visited (currentNamespace, struct) =
      let qualifiedName = qualify currentNamespace struct
      in  modifyContext (const qualifiedName) $
            if qualifiedName `elem` visited
              then
                throwErrorMsg $
                  "cyclic dependency detected ["
                  <> display (T.intercalate " -> " . coerce $ List.dropWhile (/= qualifiedName) $ List.reverse (qualifiedName : visited))
                  <>"] - structs cannot contain themselves, directly or indirectly"
              else
                forM_ (ST.structFields struct) $ \field ->
                  modifyContext (\context -> context <> "." <> getIdent field) $
                    case ST.structFieldType field of
                      ST.TRef typeRef ->
                        findDecl currentNamespace symbolTables typeRef >>= \case
                          MatchS struct -> go (qualifiedName : visited) struct
                          _             -> pure () -- The TypeRef points to an enum (or is invalid), so no further validation is needed at this point
                      _ -> pure () -- Field is not a TypeRef, no validation needed

data UnpaddedStructField = UnpaddedStructField
  { unpaddedStructFieldIdent :: !Ident
  , unpaddedStructFieldType  :: !StructFieldType
  } deriving (Show, Eq)

validateStruct ::
     forall m. (MonadState [(Namespace, StructDecl)] m, ValidationCtx m)
  => FileTree Stage2
  -> (Namespace, ST.StructDecl)
  -> m (Namespace, StructDecl)
validateStruct symbolTables (currentNamespace, struct) =
  modifyContext (\_ -> qualify currentNamespace struct) $ do
    validStructs <- get
    -- Check if this struct has already been validated in a previous iteration
    case find (\(ns, s) -> ns == currentNamespace && getIdent s == getIdent struct) validStructs of
      Just match -> pure match
      Nothing -> do
        checkDuplicateFields
        checkUndeclaredAttributes struct

        fields <- traverse validateStructField (ST.structFields struct)
        let naturalAlignment = maximum (structFieldAlignment <$> fields)
        forceAlignAttrVal <- getForceAlignAttr
        forceAlign <- traverse (validateForceAlign naturalAlignment) forceAlignAttrVal
        let alignment = fromMaybe naturalAlignment forceAlign

        -- In order to calculate the padding between fields, we must first know the fields' and the struct's
        -- alignment. Which means we must first validate all the struct's fields, and then do a second
        -- pass to calculate the padding.
        let (size, paddedFields) = addFieldPadding alignment fields

        let validStruct = StructDecl
              { structIdent      = getIdent struct
              , structAlignment  = alignment
              , structSize       = size
              , structFields     = paddedFields
              }
        modify ((currentNamespace, validStruct) :)
        pure (currentNamespace, validStruct)

  where
    invalidStructFieldType = "struct fields may only be integers, floating point, bool, enums, or other structs"

    -- | Calculates how much padding each field needs, and returns the struct's total size
    -- and a list of fields with padding information.
    addFieldPadding :: Alignment -> NonEmpty UnpaddedStructField -> (InlineSize, NonEmpty StructField)
    addFieldPadding structAlignment unpaddedFields =
      (size, NE.fromList (reverse paddedFields))
      where

        (size, paddedFields) = go 0 [] (NE.toList unpaddedFields)

        go :: InlineSize -> [StructField] -> [UnpaddedStructField] -> (InlineSize, [StructField])
        go size paddedFields [] = (size, paddedFields)
        go size paddedFields (x : y : tail) =
          let size' = size + structFieldTypeSize (unpaddedStructFieldType x)
              nextFieldsAlignment = fromIntegral @Alignment @InlineSize (structFieldAlignment y)
              paddingNeeded = (size' `roundUpToNearestMultipleOf` nextFieldsAlignment) - size'
              size'' = size' + paddingNeeded
              paddedField = StructField
                { structFieldIdent = unpaddedStructFieldIdent x
                -- NOTE: it is safe to narrow `paddingNeeded` to a word8 here because it's always smaller than `nextFieldsAlignment`
                , structFieldPadding = fromIntegral @InlineSize @Word8 paddingNeeded
                , structFieldOffset = coerce size
                , structFieldType = unpaddedStructFieldType x
                }
          in  go size'' (paddedField : paddedFields) (y : tail)
        go size paddedFields [x] =
          let size' = size + structFieldTypeSize (unpaddedStructFieldType x)
              structAlignment' = fromIntegral @Alignment @InlineSize structAlignment
              paddingNeeded = (size' `roundUpToNearestMultipleOf` structAlignment') - size'
              size'' = size' + paddingNeeded
              paddedField = StructField
                { structFieldIdent = unpaddedStructFieldIdent x
                -- NOTE: it is safe to narrow `paddingNeeded` to a word8 here because it's always smaller than `nextFieldsAlignment`
                , structFieldPadding = fromIntegral @InlineSize @Word8 paddingNeeded
                , structFieldOffset = coerce size
                , structFieldType = unpaddedStructFieldType x
                }
          in  (size'', paddedField : paddedFields)

    validateStructField :: ST.StructField -> m UnpaddedStructField
    validateStructField sf =
      modifyContext (\context -> context <> "." <> getIdent sf) $ do
        checkUnsupportedAttributes sf
        checkUndeclaredAttributes sf
        structFieldType <- validateStructFieldType (ST.structFieldType sf)
        pure $ UnpaddedStructField
          { unpaddedStructFieldIdent = getIdent sf
          , unpaddedStructFieldType = structFieldType
          }

    validateStructFieldType :: ST.Type -> m StructFieldType
    validateStructFieldType structFieldType =
      case structFieldType of
        ST.TInt8 -> pure SInt8
        ST.TInt16 -> pure SInt16
        ST.TInt32 -> pure SInt32
        ST.TInt64 -> pure SInt64
        ST.TWord8 -> pure SWord8
        ST.TWord16 -> pure SWord16
        ST.TWord32 -> pure SWord32
        ST.TWord64 -> pure SWord64
        ST.TFloat -> pure SFloat
        ST.TDouble -> pure SDouble
        ST.TBool -> pure SBool
        ST.TString -> throwErrorMsg invalidStructFieldType
        ST.TVector _ -> throwErrorMsg invalidStructFieldType
        ST.TRef typeRef ->
          findDecl currentNamespace symbolTables typeRef >>= \case
            MatchE (enumNamespace, enum) ->
              pure (SEnum (TypeRef enumNamespace (getIdent enum)) (enumType enum))
            MatchS (nestedNamespace, nestedStruct) ->
              -- if this is a reference to a struct, we need to validate it first
              SStruct <$> validateStruct symbolTables (nestedNamespace, nestedStruct)
            _ -> throwErrorMsg invalidStructFieldType

    checkUnsupportedAttributes :: ST.StructField -> m ()
    checkUnsupportedAttributes structField = do
      when (hasAttribute deprecatedAttr (ST.structFieldMetadata structField)) $
        throwErrorMsg "can't deprecate fields in a struct"
      when (hasAttribute requiredAttr (ST.structFieldMetadata structField)) $
        throwErrorMsg "struct fields are already required, the 'required' attribute is redundant"
      when (hasAttribute idAttr (ST.structFieldMetadata structField)) $
        throwErrorMsg "struct fields cannot be reordered using the 'id' attribute"

    getForceAlignAttr :: m (Maybe Integer)
    getForceAlignAttr = findIntAttr forceAlignAttr (ST.structMetadata struct)

    validateForceAlign :: Alignment -> Integer -> m Alignment
    validateForceAlign naturalAlignment forceAlign =
      if isPowerOfTwo forceAlign
        && inRange (fromIntegral @Alignment @Integer naturalAlignment, 16) forceAlign
        then pure (fromIntegral @Integer @Alignment forceAlign)
        else throwErrorMsg $
              "force_align must be a power of two integer ranging from the struct's natural alignment (in this case, "
              <> display naturalAlignment
              <> ") to 16"

    checkDuplicateFields :: m ()
    checkDuplicateFields =
      checkDuplicateIdentifiers
        (ST.structFields struct)

----------------------------------
------------ Helpers -------------
----------------------------------
structFieldAlignment :: UnpaddedStructField -> Alignment
structFieldAlignment usf =
  case unpaddedStructFieldType usf of
    SInt8 -> int8Size
    SInt16 -> int16Size
    SInt32 -> int32Size
    SInt64 -> int64Size
    SWord8 -> word8Size
    SWord16 -> word16Size
    SWord32 -> word32Size
    SWord64 -> word64Size
    SFloat -> floatSize
    SDouble -> doubleSize
    SBool -> boolSize
    SEnum _ enumType -> enumAlignment enumType
    SStruct (_, nestedStruct) -> structAlignment nestedStruct

enumAlignment :: EnumType -> Alignment
enumAlignment = Alignment . enumSize

-- | The size of an enum is either 1, 2, 4 or 8 bytes, so its size fits in a Word8
enumSize :: EnumType -> Word8
enumSize e =
  case e of
    EInt8 -> int8Size
    EInt16 -> int16Size
    EInt32 -> int32Size
    EInt64 -> int64Size
    EWord8 -> word8Size
    EWord16 -> word16Size
    EWord32 -> word32Size
    EWord64 -> word64Size

structFieldTypeSize :: StructFieldType -> InlineSize
structFieldTypeSize sft =
  case sft of
    SInt8 -> int8Size
    SInt16 -> int16Size
    SInt32 -> int32Size
    SInt64 -> int64Size
    SWord8 -> word8Size
    SWord16 -> word16Size
    SWord32 -> word32Size
    SWord64 -> word64Size
    SFloat -> floatSize
    SDouble -> doubleSize
    SBool -> boolSize
    SEnum _ enumType -> fromIntegral @Word8 @InlineSize (enumSize enumType)
    SStruct (_, nestedStruct) -> structSize nestedStruct

checkDuplicateIdentifiers :: (ValidationCtx m, Foldable f, Functor f, HasIdent a) => f a -> m ()
checkDuplicateIdentifiers xs =
  case findDups (getIdent <$> xs) of
    [] -> pure ()
    dups ->
      throwErrorMsg $
        display dups <> " declared more than once"
  where
    findDups :: (Foldable f, Functor f, Ord a) => f a -> [a]
    findDups xs = Map.keys $ Map.filter (>1) $ occurrences xs

    occurrences :: (Foldable f, Functor f, Ord a) => f a -> Map a (Sum Int)
    occurrences xs =
      Map.unionsWith (<>) $ Foldable.toList $ fmap (\x -> Map.singleton x (Sum 1)) xs

checkUndeclaredAttributes :: (ValidationCtx m, HasMetadata a) => a -> m ()
checkUndeclaredAttributes a = do
  allAttributes <- asks validationStateAllAttributes
  forM_ (Map.keys . ST.unMetadata . getMetadata $ a) $ \attr ->
    when (coerce attr `Set.notMember` allAttributes) $
      throwErrorMsg $ "user defined attributes must be declared before use: " <> display attr

hasAttribute :: Text -> ST.Metadata -> Bool
hasAttribute name (ST.Metadata attrs) = Map.member name attrs

findIntAttr :: ValidationCtx m => Text -> ST.Metadata -> m (Maybe Integer)
findIntAttr name (ST.Metadata attrs) =
  case Map.lookup name attrs of
    Nothing                  -> pure Nothing
    Just Nothing             -> err
    Just (Just (ST.AttrI i)) -> pure (Just i)
    Just (Just (ST.AttrS t)) ->
      case readMaybe @Integer (T.unpack t) of
        Just i  -> pure (Just i)
        Nothing -> err
  where
    err =
      throwErrorMsg $
        "expected attribute '"
        <> display name
        <> "' to have an integer value, e.g. '"
        <> display name
        <> ": 123'"

findStringAttr :: ValidationCtx m => Text -> ST.Metadata -> m (Maybe Text)
findStringAttr name (ST.Metadata attrs) =
  case Map.lookup name attrs of
    Nothing                  -> pure Nothing
    Just (Just (ST.AttrS s)) -> pure (Just s)
    Just _ ->
      throwErrorMsg $
        "expected attribute '"
        <> display name
        <> "' to have a string value, e.g. '"
        <> display name
        <> ": \"abc\"'"

throwErrorMsg :: ValidationCtx m => String -> m a
throwErrorMsg msg = do
  context <- asks validationStateCurrentContext
  if context == ""
    then throwError msg
    else throwError $ "[" <> display context <> "]: " <> msg


isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = (n .&. (n - 1)) == 0

roundUpToNearestMultipleOf :: Integral n => n -> n -> n
roundUpToNearestMultipleOf x y =
  case x `rem` y of
    0         -> x
    remainder -> (y - remainder) + x
