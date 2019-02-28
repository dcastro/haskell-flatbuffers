{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlatBuffers.Internal.Compiler.SyntaxTree where

import           Data.List.NonEmpty (NonEmpty)
import           Data.String        (IsString)
import           Data.Text          (Text)

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
  } deriving (Show, Eq, IsString)

newtype Include = Include
  { unInclude :: StringLiteral
  } deriving (Show, Eq, IsString)

newtype StringLiteral = StringLiteral
  { unStringLiteral :: Text
  } deriving (Show, Eq, IsString)

newtype IntLiteral = IntLiteral
  { unIntLiteral :: Integer
  } deriving (Show, Eq, Num, Enum, Ord, Real, Integral)

newtype NumberLiteral = NumberLiteral
  { unNumberLiteral :: String
  } deriving (Show, Eq, IsString)

data Literal
  = LiteralN NumberLiteral
  | LiteralS StringLiteral
  deriving (Show, Eq)

data DefaultVal
  = DefaultN NumberLiteral
  | DefaultB Bool
  deriving (Show, Eq)

newtype Metadata = Metadata
  { unMetadata :: NonEmpty (Ident, Maybe Literal)
  } deriving (Show, Eq)

newtype NamespaceDecl = NamespaceDecl
  { unNamespace :: NonEmpty Ident
  } deriving (Show, Eq)

data TableDecl = TableDecl
  { tableDeclIdent    :: Ident
  , tableDeclMetadata :: Maybe Metadata
  , tableDeclFields   :: [TableField]
  } deriving (Show, Eq)

data TableField = TableField
  { tableFieldIdent    :: Ident
  , tableFieldType     :: Type
  , tableFieldDefault  :: Maybe DefaultVal
  , tableFieldMetadata :: Maybe Metadata
  } deriving (Show, Eq)

data StructDecl = StructDecl
  { structDeclIdent    :: Ident
  , structDeclMetadata :: Maybe Metadata
  , structDeclFields   :: NonEmpty StructField
  } deriving (Show, Eq)

data StructField = StructField
  { structFieldIdent    :: Ident
  , structFieldType     :: Type
  , structFieldMetadata :: Maybe Metadata
  } deriving (Show, Eq)

data EnumDecl = EnumDecl
  { enumDeclIdent    :: Ident
  , enumDeclType     :: Type
  , enumDeclMetadata :: Maybe Metadata
  , enumDeclVals     :: NonEmpty EnumValDecl
  } deriving (Show, Eq)

data EnumValDecl = EnumValDecl
  { enumValDeclIdent   :: Ident
  , enumValDeclLiteral :: Maybe IntLiteral
  } deriving (Show, Eq)

data UnionDecl = UnionDecl
  { unionDeclIdent    :: Ident
  , unionDeclMetadata :: Maybe Metadata
  , unionDeclVals     :: NonEmpty UnionValDecl
  } deriving (Show, Eq)

data UnionValDecl = UnionValDecl
  { unionValDeclAlias :: Maybe Ident
  , unionValDeclType  :: TypeRef
  } deriving (Show, Eq)

data Type
  -- numeric
  = Tint8
  | Tint16
  | Tint32
  | Tint64
  | Tword8
  | Tword16
  | Tword32
  | Tword64
  -- floating point
  | Tfloat
  | Tdouble
  -- others
  | Tbool
  | Tstring
  | Tvector Type
  | Tref TypeRef
  deriving (Show, Eq)

data TypeRef = TypeRef
  { typeRefNamespace :: Namespace
  , typeRefIdent     :: Ident
  } deriving (Show, Eq)

newtype Namespace = Namespace [Ident]
  deriving (Show, Eq)
  
newtype RootDecl = RootDecl TypeRef
  deriving (Show, Eq)

newtype FileIdentifierDecl = FileIdentifierDecl StringLiteral
  deriving (Show, Eq, IsString)

newtype AttributeDecl = AttributeDecl Ident
  deriving (Show, Eq, IsString)
