{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module FlatBuffers.Internal.Compiler.SemanticAnalysis where

import           Control.Applicative                      ((<|>))
import           Control.Monad                            (when)
import           Control.Monad.Except                     (MonadError,
                                                           throwError)
import           Control.Monad.State                      (State, evalState,
                                                           execStateT, get,
                                                           modify, put)
import           Control.Monad.State.Class                (MonadState)
import           Data.Coerce                              (coerce)
import           Data.Foldable                            (find, maximum,
                                                           traverse_)
import           Data.Functor                             ((<&>))
import           Data.Int
import           Data.Ix                                  (inRange)
import           Data.List.NonEmpty                       (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                       as NE
import           Data.Map.Strict                          (Map)
import qualified Data.Map.Strict                          as Map
import           Data.Maybe                               (fromMaybe)
import           Data.Monoid                              (Sum (..))
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Tree                                as Tree
import           Data.Word
import           FlatBuffers.Constants                    (InlineSize (..))
import           FlatBuffers.Internal.Compiler.SyntaxTree (Ident, Namespace,
                                                           Schema, qualify)
import qualified FlatBuffers.Internal.Compiler.SyntaxTree as ST
import           FlatBuffers.Internal.Util                (Display (..),
                                                           isPowerOfTwo)

type ParseCtx = MonadError Text

type Required = Bool

newtype DefaultVal a = DefaultVal (Maybe a)

-- | Unvalidated type declarations, paired with their respective namespaces
data DeclsWithNamespace = DeclsWithNamespace
  { tables  :: [(Namespace, ST.TableDecl)]
  , structs :: [(Namespace, ST.StructDecl)]
  , enums   :: [(Namespace, ST.EnumDecl)]
  , unions  :: [(Namespace, ST.UnionDecl)]
  }

instance Semigroup DeclsWithNamespace where
  DeclsWithNamespace t1 s1 e1 u1 <> DeclsWithNamespace t2 s2 e2 u2 =
    DeclsWithNamespace (t1 <> t2) (s1 <> s2) (e1 <> e2) (u1 <> u2)

instance Monoid DeclsWithNamespace where
  mempty = DeclsWithNamespace [] [] [] []

-- | Semantically valid type declarations
data ValidatedDecls = ValidatedDecls
  { validatedEnums   :: [EnumDecl]
  , validatedStructs :: [StructDecl]
  } deriving (Show, Eq)

instance Semigroup ValidatedDecls where
  ValidatedDecls e1 s1 <> ValidatedDecls e2 s2 =
    ValidatedDecls (e1 <> e2) (s1 <> s2)

instance Monoid ValidatedDecls where
  mempty = ValidatedDecls [] []

-- | Takes a collection of schemas, and pairs each type declaration with its corresponding namespace
pairDeclsWithNamespaces :: Tree.Tree Schema -> DeclsWithNamespace
pairDeclsWithNamespaces = foldMap (pairDeclsWithNamespaces' . ST.decls)
  where
    pairDeclsWithNamespaces' :: [ST.Decl] -> DeclsWithNamespace
    pairDeclsWithNamespaces' = snd . foldl go ("", mempty)

    go :: (Namespace, DeclsWithNamespace) -> ST.Decl -> (Namespace, DeclsWithNamespace)
    go (currentNamespace, decls) decl =
      case decl of
        ST.DeclN (ST.NamespaceDecl newNamespace) -> (newNamespace, decls)
        ST.DeclT table  -> (currentNamespace, decls <> DeclsWithNamespace [(currentNamespace, table)] [] [] [])
        ST.DeclS struct -> (currentNamespace, decls <> DeclsWithNamespace [] [(currentNamespace, struct)] [] [])
        ST.DeclE enum   -> (currentNamespace, decls <> DeclsWithNamespace [] [] [(currentNamespace, enum)] [])
        ST.DeclU union  -> (currentNamespace, decls <> DeclsWithNamespace [] [] [] [(currentNamespace, union)])
        _               -> (currentNamespace, decls)


validateDecls :: ParseCtx m => DeclsWithNamespace -> m ValidatedDecls
validateDecls decls = do
  validatedEnums <- validateEnums (enums decls)
  validatedStructs <- validateStructs validatedEnums (structs decls)

  pure $ ValidatedDecls
    { validatedEnums = validatedEnums
    , validatedStructs = validatedStructs
    }


data EnumDecl = EnumDecl
  { enumNamespace :: Namespace
  , enumIdent     :: Ident
  , enumType      :: EnumType
  , enumVals      :: NonEmpty EnumVal
  } deriving (Show, Eq)

data EnumVal = EnumVal
  { enumValIdent :: Ident
  , enumValInt   :: Integer
  } deriving (Show, Eq)

data EnumType
  = EInt8
  | EInt16
  | EInt32
  | EInt64
  | EWord8
  | EWord16
  | EWord32
  | EWord64
  deriving (Show, Eq)

validateEnums :: forall m. ParseCtx m => [(Namespace, ST.EnumDecl)] -> m [EnumDecl]
validateEnums = traverse validateEnum

-- TODO: add support for `bit_flags` attribute
validateEnum :: forall m. ParseCtx m => (Namespace, ST.EnumDecl) -> m EnumDecl
validateEnum (currentNamespace, enum) = checkBitFlags >> checkDuplicateFields >> validEnum
  where
    qualifiedName = qualify currentNamespace (ST.enumIdent enum)

    validEnum = do
      enumType <- validateEnumType (ST.enumType enum)
      let enumVals = flip evalState Nothing . traverse mapEnumVal $ ST.enumVals enum
      validateOrder enumVals
      traverse_ (validateBounds enumType) enumVals
      pure EnumDecl
        { enumNamespace = currentNamespace
        , enumIdent = ST.enumIdent enum
        , enumType = enumType
        , enumVals = enumVals
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
      pure (EnumVal (ST.enumValIdent enumVal) thisInt)

    validateOrder :: NonEmpty EnumVal -> m ()
    validateOrder xs =
      if all (\(x, y) -> enumValInt x < enumValInt y) (NE.toList xs `zip` NE.tail xs)
        then pure ()
        else throwErrorMsg qualifiedName "enum values must be specified in ascending order"

    validateBounds :: EnumType -> EnumVal -> m ()
    validateBounds enumType enumVal =
      case enumType of
        EInt8 -> validateBounds' @Int8 enumVal
        EInt16 -> validateBounds' @Int16 enumVal
        EInt32 -> validateBounds' @Int32 enumVal
        EInt64 -> validateBounds' @Int64 enumVal
        EWord8 -> validateBounds' @Word8 enumVal
        EWord16 -> validateBounds' @Word16 enumVal
        EWord32 -> validateBounds' @Word32 enumVal
        EWord64 -> validateBounds' @Word64 enumVal

    validateBounds' :: forall a. (Integral a, Bounded a, Show a) => EnumVal -> m ()
    validateBounds' e =
      if inRange (toInteger (minBound @a), toInteger (maxBound @a)) (enumValInt e)
        then pure ()
        else throwErrorMsg qualifiedName $
              "enum value does not fit ["
              <> T.pack (show (minBound @a))
              <> "; "
              <> T.pack (show (maxBound @a))
              <> "]"

    validateEnumType :: ST.Type -> m EnumType
    validateEnumType t =
      case t of
        ST.TInt8 -> pure EInt8
        ST.TInt16 -> pure EInt16
        ST.TInt32 -> pure EInt32
        ST.TInt64 -> pure EInt64
        ST.TWord8 -> pure EWord8
        ST.TWord16 -> pure EWord16
        ST.TWord32 -> pure EWord32
        ST.TWord64 -> pure EWord64
        _          -> throwErrorMsg qualifiedName "underlying enum type must be integral"

    checkDuplicateFields :: m ()
    checkDuplicateFields =
      checkDuplicateIdentifiers qualifiedName
        (coerce . ST.enumValIdent <$> ST.enumVals enum)

    checkBitFlags :: m ()
    checkBitFlags =
      when (hasAttribute "bit_flags" (ST.enumMetadata enum)) $
        throwErrorMsg qualifiedName "`bit_flags` are not supported yet"

checkDuplicateIdentifiers :: (ParseCtx m, Foldable f, Functor f) => Ident -> f Text -> m ()
checkDuplicateIdentifiers context idents =
  case findDups idents of
    [] -> pure ()
    dups ->
      throwErrorMsg context $
      "["
      <> T.intercalate ", " dups
      <> "] declared more than once"

findDups :: (Foldable f, Functor f, Ord a) => f a -> [a]
findDups xs = Map.keys $ Map.filter (>1) $ occurrences xs

occurrences :: (Foldable f, Functor f, Ord a) => f a -> Map a (Sum Int)
occurrences xs =
  Map.unionsWith (<>) $ fmap (\x -> Map.singleton x (Sum 1)) xs

hasAttribute :: Text -> ST.Metadata -> Bool
hasAttribute name (ST.Metadata attrs) = Map.member name attrs

findIntAttr :: ParseCtx m => Ident -> Text -> ST.Metadata -> m (Maybe Integer)
findIntAttr context name (ST.Metadata attrs) =
  case Map.lookup name attrs of
    Nothing                  -> pure Nothing
    Just (Just (ST.AttrI i)) -> pure (Just i)
    Just _ ->
      throwErrorMsg context $
        "expected attribute '"
        <> name
        <> "' to have an integer value, e.g. '"
        <> name
        <> ": 123'"

findStringAttr :: ParseCtx m => Ident -> Text -> ST.Metadata -> m (Maybe Text)
findStringAttr context name (ST.Metadata attrs) =
  case Map.lookup name attrs of
    Nothing                  -> pure Nothing
    Just (Just (ST.AttrS s)) -> pure (Just s)
    Just _ ->
      throwErrorMsg context $
        "expected attribute '"
        <> name
        <> "' to have a string value, e.g. '"
        <> name
        <> ": \"abc\"'"

data Table = Table
  { tableIdent     :: Ident
  , tableNamespace :: Ident
  , tableFields    :: NonEmpty TableField
  }

data TableField = TableField
  { tableFieldType       :: TableFieldType
  , tableFieldDeprecated :: Bool
  , tableFieldId         :: Maybe Int
  }

data TableFieldType
  = TInt8 (DefaultVal Int8)
  | TInt16 (DefaultVal Int16)
  | TInt32 (DefaultVal Int32)
  | TInt64 (DefaultVal Int64)
  | TWord8 (DefaultVal Word8)
  | TWord16 (DefaultVal Word16)
  | TWord32 (DefaultVal Word32)
  | TWord64 (DefaultVal Word64)
  | TFloat (DefaultVal Float)
  | TDouble (DefaultVal Double)
  | TBool (DefaultVal Bool)
  | TEnum (DefaultVal Ident) Ident
  | TStruct Required Ident
  | TTable Required Ident
  | TUnion Required Ident
  | TString Required
  | TVector Required VectorElementType

data VectorElementType
  = VInt8
  | VInt16
  | VInt32
  | VInt64
  | VWord8
  | VWord16
  | VWord32
  | VWord64
  | VFloat
  | VDouble
  | VBool
  | VEnum Ident InlineSize
  | VStruct Ident InlineSize
  | VTable Ident
  | VUnion Ident
  | VString

data StructDecl = StructDecl
  { structNamespace  :: Namespace
  , structIdent      :: Ident
  , structAlignment  :: Word8 -- [1, 16]
  , structFields     :: NonEmpty StructField
  } deriving (Show, Eq)

data StructField = StructField
  { structFieldIdent :: Ident
  , structFieldType  :: StructFieldType
  } deriving (Show, Eq)

data StructFieldType
  = SInt8
  | SInt16
  | SInt32
  | SInt64
  | SWord8
  | SWord16
  | SWord32
  | SWord64
  | SFloat
  | SDouble
  | SBool
  | SEnum
      Namespace -- ^ The namespace of the enum that this field refers to
      Ident     -- ^ The name of the enum that this field refers to
      EnumType
  | SStruct StructDecl
  deriving (Show, Eq)


validateStructs :: ParseCtx m => [EnumDecl] -> [(Namespace, ST.StructDecl)] -> m [StructDecl]
validateStructs validatedEnums structs =
  flip execStateT [] $ traverse (validateStruct validatedEnums structs) structs

validateStruct ::
     forall m. (MonadState [StructDecl] m, ParseCtx m)
  => [EnumDecl]
  -> [(Namespace, ST.StructDecl)]
  -> (Namespace, ST.StructDecl)
  -> m StructDecl
validateStruct validatedEnums structs (currentNamespace, struct) = do
  validatedStructs <- get
  -- Check if this struct has already been validated in a previous iteration
  case find (\s -> structNamespace s == currentNamespace && structIdent s == ST.structIdent struct) validatedStructs of
    Just match -> pure match
    Nothing -> do
      checkDuplicateFields

      fields <- traverse validateStructField (ST.structFields struct)
      let naturalAlignment = maximum (structFieldAlignment <$> fields)
      forceAlignAttr <- getForceAlignAttr
      forceAlign <- traverse (validateForceAlign naturalAlignment) forceAlignAttr
      let alignment = fromMaybe naturalAlignment forceAlign

      let validatedStruct = StructDecl
            { structNamespace  = currentNamespace
            , structIdent      = ST.structIdent struct
            , structAlignment  = alignment
            , structFields     = fields
            }
      modify (validatedStruct :)
      pure validatedStruct

  where
    qualifiedName = qualify currentNamespace (ST.structIdent struct)

    validateStructField :: ST.StructField -> m StructField
    validateStructField sf = do
      checkDeprecated (ST.structFieldIdent sf) sf
      structFieldType <- validateStructFieldType (ST.structFieldIdent sf) (ST.structFieldType sf)
      pure $ StructField
        { structFieldIdent = ST.structFieldIdent sf
        , structFieldType = structFieldType
        }

    validateStructFieldType :: Ident -> ST.Type -> m StructFieldType
    validateStructFieldType structFieldIdent structFieldType =
      let 
        structFieldQualifiedName = qualifiedName <> "." <> structFieldIdent
        invalidStructFieldType = "struct fields may only be integers, floating point, bool, enums, or other structs"
      in
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
          ST.TString -> throwErrorMsg structFieldQualifiedName invalidStructFieldType
          ST.TVector _ -> throwErrorMsg structFieldQualifiedName invalidStructFieldType
          ST.TRef typeRef ->
            -- check if this is a reference to an enum
            case findDecl currentNamespace validatedEnums enumNamespace enumIdent typeRef of
              Right enum -> pure (SEnum (enumNamespace enum) (enumIdent enum) (enumType enum))
              Left _ ->
                -- check if this is a reference to a struct, and validate it
                case findDecl currentNamespace structs fst (ST.structIdent . snd) typeRef of
                  Right (nestedNamespace, nestedStruct) ->
                    SStruct <$> validateStruct validatedEnums structs (nestedNamespace, nestedStruct)
                  Left checkedNamespaces ->
                    throwErrorMsg structFieldQualifiedName $
                      "type '"
                      <> display typeRef
                      <> "' does not exist (checked in these namespaces: "
                      <> display checkedNamespaces
                      <> ") or is not allowed in a struct field"
                      <> " (struct fields may only be integers, floating point, bool, enums, or structs)"

    checkDeprecated :: Ident -> ST.StructField -> m ()
    checkDeprecated structFieldIdent structField =
      let structFieldQualifiedName = qualifiedName <> "." <> structFieldIdent
      in  when (hasAttribute "deprecated" (ST.structFieldMetadata structField)) $
            throwErrorMsg structFieldQualifiedName "can't deprecate fields in a struct"

    getForceAlignAttr :: m (Maybe Integer)
    getForceAlignAttr = findIntAttr qualifiedName "force_align" (ST.structMetadata struct)

    validateForceAlign :: Word8 -> Integer -> m Word8
    validateForceAlign naturalAlignment forceAlign =
      if isPowerOfTwo forceAlign
        && inRange (fromIntegral @Word8 @Integer naturalAlignment, 16) forceAlign
        then pure (fromIntegral @Integer @Word8 forceAlign)
        else throwErrorMsg qualifiedName $
              "force_align must be a power of two integer ranging from the struct's natural alignment (in this case, "
              <> T.pack (show naturalAlignment)
              <> ") to 16"

    checkDuplicateFields :: m ()
    checkDuplicateFields =
      checkDuplicateIdentifiers qualifiedName
        (coerce . ST.structFieldIdent <$> ST.structFields struct)

structFieldAlignment :: StructField -> Word8
structFieldAlignment sf =
  case structFieldType sf of
    SInt8 -> 1
    SInt16 -> 2
    SInt32 -> 4
    SInt64 -> 8
    SWord8 -> 1
    SWord16 -> 2
    SWord32 -> 4
    SWord64 -> 8
    SFloat -> 4
    SDouble -> 8
    SBool -> 1
    SEnum _ _ enumType -> enumAlignment enumType
    SStruct nestedStruct -> structAlignment nestedStruct

-- | The size of an enum is either 1, 2, 4 or 8 bytes, so its size fits in a Word8
enumSize :: EnumType -> Word8
enumSize e =
  case e of
    EInt8 -> 1
    EInt16 -> 2
    EInt32 -> 3
    EInt64 -> 4
    EWord8 -> 1
    EWord16 -> 2
    EWord32 -> 3
    EWord64 -> 4

enumAlignment :: EnumType -> Word8
enumAlignment = enumSize

-- | Returns a list of all the namespaces "between" the current namespace
-- and the root namespace, in that order.
-- See: https://github.com/google/flatbuffers/issues/5234#issuecomment-471680403
-- 
-- > parentNamespaces "A.B.C" == ["A.B.C", "A.B", "A", ""]
parentNamespaces :: ST.Namespace -> NonEmpty ST.Namespace
parentNamespaces (ST.Namespace ns) =
  coerce $ NE.reverse $ NE.inits ns
  

-- | Looks for a type reference in a set of type declarations.
-- If none is found, the list of namespaces in which the type reference was searched for is return.
findDecl ::
     Namespace
  -> [decl]
  -> (decl -> Namespace)
  -> (decl -> Ident)
  -> ST.TypeRef
  -> Either (NonEmpty Namespace) decl
findDecl currentNamespace decls getNamespace getIdent (ST.TypeRef refNamespace refIdent) =
  let parentNamespaces' = parentNamespaces currentNamespace
      results = do
        parentNamespace <- parentNamespaces'
        let candidateNamespace = parentNamespace <> refNamespace
        pure $ find (\decl -> getNamespace decl == candidateNamespace && getIdent decl == refIdent) decls
  in  
    case foldl1 (<|>) results of
      Just match -> Right match
      Nothing    -> Left parentNamespaces'

enumQualifiedName :: EnumDecl -> Ident
enumQualifiedName e = qualify (enumNamespace e) (enumIdent e)

structQualifiedName :: StructDecl -> Ident
structQualifiedName s = qualify (structNamespace s) (structIdent s)


throwErrorMsg :: ParseCtx m => Ident -> Text -> m a
throwErrorMsg context msg =
  throwError $ "[" <> display context <> "]: " <> msg
