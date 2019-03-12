{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module FlatBuffers.Internal.Compiler.SyntaxTree where

import           Data.List.NonEmpty        (NonEmpty)
import           Data.Map.Strict           (Map)
import           Data.String               (IsString (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           FlatBuffers.Internal.Util (Display (..))

data Schema = Schema
  { includes :: [Include]
  , decls    :: [Decl]
  } deriving (Show, Eq)

data Decl
  = DeclN NamespaceDecl
  | DeclT TableDecl
  | DeclS StructDecl
  | DeclE EnumDecl
  | DeclU UnionDecl
  | DeclR RootDecl
  | DeclFI FileIdentifierDecl
  | DeclA AttributeDecl
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

newtype NumberLiteral = NumberLiteral
  { unNumberLiteral :: String
  } deriving newtype (Show, Eq, IsString)

data AttributeVal
  = AttrI Integer
  | AttrS Text
  deriving (Show, Eq)

data DefaultVal
  = DefaultN NumberLiteral
  | DefaultB Bool
  | DefaultI Ident
  deriving (Show, Eq)

newtype Metadata = Metadata
  { unMetadata :: Map Text (Maybe AttributeVal)
  } deriving newtype (Show, Eq)

newtype NamespaceDecl = NamespaceDecl
  { unNamespaceDecl :: Namespace
  } deriving newtype (Show, Eq, IsString)

data TableDecl = TableDecl
  { tableIdent    :: Ident
  , tableMetadata :: Metadata
  , tableFields   :: [TableField]
  } deriving (Show, Eq)

data TableField = TableField
  { tableFieldIdent    :: Ident
  , tableFieldType     :: Type
  , tableFieldDefault  :: Maybe DefaultVal
  , tableFieldMetadata :: Metadata
  } deriving (Show, Eq)

data StructDecl = StructDecl
  { structIdent    :: Ident
  , structMetadata :: Metadata
  , structFields   :: NonEmpty StructField
  } deriving (Show, Eq)

data StructField = StructField
  { structFieldIdent    :: Ident
  , structFieldType     :: Type
  , structFieldMetadata :: Metadata
  } deriving (Show, Eq)

data EnumDecl = EnumDecl
  { enumIdent    :: Ident
  , enumType     :: Type
  , enumMetadata :: Metadata
  , enumVals     :: NonEmpty EnumVal
  } deriving (Show, Eq)

data EnumVal = EnumVal
  { enumValIdent   :: Ident
  , enumValLiteral :: Maybe IntLiteral
  } deriving (Show, Eq)

data UnionDecl = UnionDecl
  { unionIdent    :: Ident
  , unionMetadata :: Metadata
  , unionVals     :: NonEmpty UnionVal
  } deriving (Show, Eq)

data UnionVal = UnionVal
  { unionValAlias :: Maybe Ident
  , unionValType  :: TypeRef
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
  | TVector Type
  | TRef TypeRef
  deriving (Show, Eq)

data TypeRef = TypeRef
  { typeRefNamespace :: Namespace
  , typeRefIdent     :: Ident
  } deriving (Show, Eq)

newtype RootDecl = RootDecl TypeRef
  deriving newtype (Show, Eq)

newtype FileIdentifierDecl = FileIdentifierDecl StringLiteral
  deriving newtype (Show, Eq, IsString)

newtype AttributeDecl = AttributeDecl Text
  deriving newtype (Show, Eq, IsString)

newtype Namespace = Namespace {unNamespace :: [Text] }
  deriving newtype (Eq, Ord, Semigroup)

instance Display Namespace where
  display (Namespace ns) = T.intercalate "." ns

instance Show Namespace where
  show = show . T.unpack . display

instance IsString Namespace where
  fromString "" = Namespace []
  fromString s = Namespace $ filter (/= "") $ T.splitOn "." $ T.pack s
