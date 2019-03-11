{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module FlatBuffers.Internal.Compiler.SyntaxTree where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict    (Map)
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
  } deriving (Show, Eq, IsString, Ord)

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
  } deriving (Show, Eq)

newtype NamespaceDecl = NamespaceDecl
  { unNamespace :: Namespace
  } deriving (Show, Eq, IsString)

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
  deriving (Show, Eq)

newtype FileIdentifierDecl = FileIdentifierDecl StringLiteral
  deriving (Show, Eq, IsString)

newtype AttributeDecl = AttributeDecl Text
  deriving (Show, Eq, IsString)

newtype Namespace = Namespace Text
  deriving (Show, Eq, IsString, Ord)

qualify :: Namespace -> Ident -> Ident
qualify "" i = i
qualify (Namespace ns) (Ident i) = Ident (ns <> "." <> i)
