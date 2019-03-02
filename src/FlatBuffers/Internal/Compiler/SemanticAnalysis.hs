{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module FlatBuffers.Internal.Compiler.SemanticAnalysis where

import           Control.Monad.Except
import           Control.Monad.State                      (State, evalState,
                                                           get, put)
import           Data.Coerce                              (coerce)
import           Data.Foldable
import           Data.Functor                             (($>), (<&>))
import           Data.Int
import           Data.Ix                                  (inRange)
import           Data.List.NonEmpty                       (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                       as NE
import           Data.Map.Strict                          (Map)
import qualified Data.Map.Strict                          as M
import           Data.Monoid                              (Sum (..))
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Data.Tree                                (Tree (..))
import qualified Data.Tree                                as Tree
import           Data.Word
import           FlatBuffers.Constants                    (InlineSize (..))
import           FlatBuffers.Internal.Compiler.SyntaxTree (Ident, Namespace,
                                                           Schema)
import qualified FlatBuffers.Internal.Compiler.SyntaxTree as ST

type ParseCtx = MonadError Text

type Required = Bool

newtype VDefaultVal a = VDefaultVal (Maybe a)

data EnumDecl = EnumDecl
  { enumIdent :: Ident
  , enumType  :: EnumType
  , enumVals  :: NonEmpty EnumVal
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

-- TODO: add support for `bit_flags` attribute
validateEnum :: forall m. ParseCtx m => Namespace -> ST.EnumDecl -> m EnumDecl
validateEnum ns enum = checkBitFlags >> checkDups >> validEnum
  where
    qualifiedName = "[" <> ST.unIdent (ST.qualify ns (ST.enumIdent enum)) <> "]"

    validEnum = do
      enumType <- validateEnumType (ST.enumType enum)
      let enumVals = flip evalState Nothing . traverse mapEnumVal $ ST.enumVals enum
      validateOrder enumVals
      traverse_ (validateBounds enumType) enumVals
      pure EnumDecl
        { enumIdent = ST.enumIdent enum
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
        else throwError $ qualifiedName <> ": enum values must be specified in ascending order"

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
        else throwError $ qualifiedName <> ": enum value does not fit [" <> T.pack (show (minBound @a)) <> "; " <> T.pack (show (maxBound @a)) <> "]"

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
        _          -> throwError $ qualifiedName <> ": underlying enum type must be integral"

    checkDups :: m ()
    checkDups =
      case findDupsBy ST.enumValIdent (ST.enumVals enum) of
        [] -> pure ()
        dups ->
          throwError $
          qualifiedName <>
          ": fields [" <> T.intercalate ", " (coerce dups) <>
          "] declared more than once"

    checkBitFlags :: m ()
    checkBitFlags =
      if any (\(x, _) -> x == "bit_flags") (ST.unMetadata (ST.enumMetadata enum))
        then throwError $ qualifiedName <> ": `bit_flags` are not supported yet"
        else pure ()


findDupsBy :: (Foldable f, Functor f, Ord b) => (a -> b) -> f a -> [b]
findDupsBy f xs = M.keys $ M.filter (>1) $ occurrences $ fmap f xs

occurrences :: (Foldable f, Functor f, Ord a) => f a -> Map a (Sum Int)
occurrences xs =
  M.unionsWith (<>) $ fmap (\x -> M.singleton x (Sum 1)) xs


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
  = TInt8 (VDefaultVal Int8)
  | TInt16 (VDefaultVal Int16)
  | TInt32 (VDefaultVal Int32)
  | TInt64 (VDefaultVal Int64)
  | TWord8 (VDefaultVal Word8)
  | TWord16 (VDefaultVal Word16)
  | TWord32 (VDefaultVal Word32)
  | TWord64 (VDefaultVal Word64)
  | TFloat (VDefaultVal Float)
  | TDouble (VDefaultVal Double)
  | TBool (VDefaultVal Bool)
  | TEnum (VDefaultVal Ident) Ident
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

data Struct = Struct
  { structIdent      :: Ident
  , structNamespace  :: Text
  , structFields     :: NonEmpty StructField
  , structForceAlign :: Word64 -- TODO: what should the size of this actually be?
  }

newtype StructField = StructField
  { structFieldType :: StructFieldType
  }

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
  | SEnum Ident Word8 -- The size of an enum is either 1, 2, 4 or 8 bytes, so its size fits in a Word8
  | SStruct Struct
