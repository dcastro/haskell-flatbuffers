{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.Internal.Compiler.SyntaxTree where

import           Data.List.NonEmpty                    ( NonEmpty )
import           Data.Map.Strict                       ( Map )
import           Data.Scientific                       ( Scientific )
import           Data.String                           ( IsString(..) )
import           Data.Text                             ( Text )
import qualified Data.Text                             as T

import           FlatBuffers.Internal.Compiler.Display ( Display(..) )

data FileTree a = FileTree
  { fileTreeFilePath :: !FilePath
  , fileTreeRoot     :: !a
  , fileTreeForest   :: !(Map FilePath a)
  }
  deriving (Show, Eq, Foldable, Functor, Traversable)

data Schema = Schema
  { includes :: ![Include]
  , decls    :: ![Decl]
  } deriving (Show, Eq)

data Decl
  = DeclN !NamespaceDecl
  | DeclT !TableDecl
  | DeclS !StructDecl
  | DeclE !EnumDecl
  | DeclU !UnionDecl
  | DeclR !RootDecl
  | DeclFI !FileIdentifierDecl
  | DeclA !AttributeDecl
  deriving (Show, Eq)

newtype Ident = Ident
  { unIdent :: Text
  } deriving newtype (Show, Eq, IsString, Ord, Semigroup, Display)

newtype Include = Include
  { unInclude :: StringLiteral
  } deriving newtype (Show, Eq, IsString)

newtype StringLiteral = StringLiteral
  { unStringLiteral :: Text
  } deriving newtype (Show, Eq, IsString)

newtype IntLiteral = IntLiteral
  { unIntLiteral :: Integer
  } deriving newtype (Show, Eq, Num, Enum, Ord, Real, Integral)

data AttributeVal
  = AttrI !Integer
  | AttrS !Text
  deriving (Show, Eq)

data DefaultVal
  = DefaultNum !Scientific
  | DefaultBool !Bool
  | DefaultRef !(NonEmpty Text)
  deriving (Show, Eq)

newtype Metadata = Metadata
  { unMetadata :: Map Text (Maybe AttributeVal)
  } deriving newtype (Show, Eq)

newtype NamespaceDecl = NamespaceDecl
  { unNamespaceDecl :: Namespace
  } deriving newtype (Show, Eq, IsString)

data TableDecl = TableDecl
  { tableIdent    :: !Ident
  , tableMetadata :: !Metadata
  , tableFields   :: ![TableField]
  } deriving (Show, Eq)

data TableField = TableField
  { tableFieldIdent    :: !Ident
  , tableFieldType     :: !Type
  , tableFieldDefault  :: !(Maybe DefaultVal)
  , tableFieldMetadata :: !Metadata
  } deriving (Show, Eq)

data StructDecl = StructDecl
  { structIdent    :: !Ident
  , structMetadata :: !Metadata
  , structFields   :: !(NonEmpty StructField)
  } deriving (Show, Eq)

data StructField = StructField
  { structFieldIdent    :: !Ident
  , structFieldType     :: !Type
  , structFieldMetadata :: !Metadata
  } deriving (Show, Eq)

data EnumDecl = EnumDecl
  { enumIdent    :: !Ident
  , enumType     :: !Type
  , enumMetadata :: !Metadata
  , enumVals     :: !(NonEmpty EnumVal)
  } deriving (Show, Eq)

data EnumVal = EnumVal
  { enumValIdent   :: !Ident
  , enumValLiteral :: !(Maybe IntLiteral)
  } deriving (Show, Eq)

data UnionDecl = UnionDecl
  { unionIdent    :: !Ident
  , unionMetadata :: !Metadata
  , unionVals     :: !(NonEmpty UnionVal)
  } deriving (Show, Eq)

data UnionVal = UnionVal
  { unionValIdent   :: !(Maybe Ident)
  , unionValTypeRef :: !TypeRef
  } deriving (Show, Eq)

data Type
  -- numeric
  = TInt8
  | TInt16
  | TInt32
  | TInt64
  | TWord8
  | TWord16
  | TWord32
  | TWord64
  -- floating point
  | TFloat
  | TDouble
  -- others
  | TBool
  | TString
  | TRef !TypeRef
  | TVector !Type
  deriving (Show, Eq)

data TypeRef = TypeRef
  { typeRefNamespace :: !Namespace
  , typeRefIdent     :: !Ident
  } deriving (Show, Eq)

instance Display TypeRef where
  display (TypeRef ns id) = display (qualify ns id)

newtype RootDecl = RootDecl TypeRef
  deriving newtype (Show, Eq)

newtype FileIdentifierDecl = FileIdentifierDecl Text
  deriving newtype (Show, Eq, IsString)

newtype AttributeDecl = AttributeDecl Text
  deriving newtype (Show, Eq, IsString, Ord)

newtype Namespace = Namespace {unNamespace :: [Text] }
  deriving newtype (Eq, Ord, Semigroup)

instance Display Namespace where
  display (Namespace ns) = display $ T.intercalate "." ns

instance Show Namespace where
  show = show . display

instance IsString Namespace where
  fromString "" = Namespace []
  fromString s = Namespace $ filter (/= "") $ T.splitOn "." $ T.pack s

qualify :: HasIdent a => Namespace -> a -> Ident
qualify "" a = getIdent a
qualify ns a = Ident (T.pack (display ns <> "." <> display (getIdent a)))

class HasIdent a where
  getIdent :: a -> Ident

instance HasIdent Ident       where getIdent = id
instance HasIdent EnumDecl    where getIdent = enumIdent
instance HasIdent EnumVal     where getIdent = enumValIdent
instance HasIdent StructDecl  where getIdent = structIdent
instance HasIdent StructField where getIdent = structFieldIdent
instance HasIdent TableDecl   where getIdent = tableIdent
instance HasIdent TableField  where getIdent = tableFieldIdent
instance HasIdent UnionDecl   where getIdent = unionIdent


class HasMetadata a where
  getMetadata :: a -> Metadata

instance HasMetadata EnumDecl    where getMetadata = enumMetadata
instance HasMetadata StructDecl  where getMetadata = structMetadata
instance HasMetadata StructField where getMetadata = structFieldMetadata
instance HasMetadata TableDecl   where getMetadata = tableMetadata
instance HasMetadata TableField  where getMetadata = tableFieldMetadata
instance HasMetadata UnionDecl   where getMetadata = unionMetadata
