module FlatBuffers.Internal.Compiler.ValidSyntaxTree
  ( -- * Re-exports from `FlatBuffers.Internal.Compiler.SyntaxTree`
    FlatBuffers.Internal.Compiler.SyntaxTree.Namespace(..)
  , FlatBuffers.Internal.Compiler.SyntaxTree.Ident(..)
  , FlatBuffers.Internal.Compiler.SyntaxTree.TypeRef(..)
  , FlatBuffers.Internal.Compiler.SyntaxTree.HasIdent(..)
  -- * Enums
  , EnumDecl(..)
  , EnumVal(..)
  , EnumType(..)
  -- * Structs
  , StructDecl(..)
  , StructField(..)
  , StructFieldType(..)
  -- * Tables
  , DefaultVal(..)
  , Required(..)
  , IsRoot(..)
  , TableDecl(..)
  , TableField(..)
  , TableFieldType(..)
  , VectorElementType(..)
  -- * Unions
  , UnionDecl(..)
  , UnionVal(..)
  ) where

import Data.Bits (Bits)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Word

import FlatBuffers.Internal.Compiler.SyntaxTree
  (HasIdent(..), Ident(..), Namespace(..), TypeRef(..))
import FlatBuffers.Internal.Types

instance HasIdent EnumDecl    where getIdent = enumIdent
instance HasIdent EnumVal     where getIdent = enumValIdent
instance HasIdent StructDecl  where getIdent = structIdent
instance HasIdent StructField where getIdent = structFieldIdent
instance HasIdent TableDecl   where getIdent = tableIdent
instance HasIdent TableField  where getIdent = tableFieldIdent
instance HasIdent UnionDecl   where getIdent = unionIdent
instance HasIdent UnionVal    where getIdent = unionValIdent

----------------------------------
------------- Enums --------------
----------------------------------
data EnumDecl = EnumDecl
  { enumIdent    :: !Ident
  , enumType     :: !EnumType
  , enumBitFlags :: !Bool
  , enumVals     :: !(NonEmpty EnumVal)
  } deriving (Show, Eq)

data EnumVal = EnumVal
  { enumValIdent :: !Ident
  , enumValInt   :: !Integer
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

----------------------------------
------------ Structs -------------
----------------------------------
data StructDecl = StructDecl
  { structIdent     :: !Ident
  , structAlignment :: !Alignment
  , structSize      :: !InlineSize
  , structFields    :: !(NonEmpty StructField)
  } deriving (Show, Eq)

data StructField = StructField
  { structFieldIdent   :: !Ident
  , structFieldPadding :: !Word8  -- ^ How many zeros to write after this field.
  , structFieldOffset  :: !Word16 -- ^ This field's offset from the struct's root.
  , structFieldType    :: !StructFieldType
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
      !TypeRef
      !EnumType
  | SStruct !(Namespace, StructDecl)
  deriving (Show, Eq)

----------------------------------
------------ Tables --------------
----------------------------------
newtype DefaultVal a = DefaultVal a
  deriving newtype (Eq, Show, Num, IsString, Ord, Enum, Real, Integral, Fractional, Bits)

data Required = Req | Opt
  deriving (Eq, Show)

data IsRoot
  = NotRoot              -- ^ This table is not the root table.
  | IsRoot !(Maybe Text) -- ^ This table is the root table, and has an optional file identifier.
  deriving (Eq, Show)

data TableDecl = TableDecl
  { tableIdent  :: !Ident
  , tableIsRoot :: !IsRoot
  , tableFields :: ![TableField]
  } deriving (Eq, Show)

data TableField = TableField
  { tableFieldId         :: !Integer
  , tableFieldIdent      :: !Ident
  , tableFieldType       :: !TableFieldType
  , tableFieldDeprecated :: !Bool
  } deriving (Eq, Show)

data TableFieldType
  = TInt8   !(DefaultVal Integer)
  | TInt16  !(DefaultVal Integer)
  | TInt32  !(DefaultVal Integer)
  | TInt64  !(DefaultVal Integer)
  | TWord8  !(DefaultVal Integer)
  | TWord16 !(DefaultVal Integer)
  | TWord32 !(DefaultVal Integer)
  | TWord64 !(DefaultVal Integer)
  | TFloat  !(DefaultVal Scientific)
  | TDouble !(DefaultVal Scientific)
  | TBool   !(DefaultVal Bool)
  | TString !Required
  | TEnum   !TypeRef !EnumType !(DefaultVal Integer)
  | TStruct !TypeRef !Required
  | TTable  !TypeRef !Required
  | TUnion  !TypeRef !Required
  | TVector !Required !VectorElementType
  deriving (Eq, Show)

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
  | VString
  | VEnum   !TypeRef !EnumType
  | VStruct !TypeRef
  | VTable  !TypeRef
  | VUnion  !TypeRef
  deriving (Eq, Show)

----------------------------------
------------ Unions --------------
----------------------------------
data UnionDecl = UnionDecl
  { unionIdent :: !Ident
  , unionVals  :: !(NonEmpty UnionVal)
  } deriving (Show, Eq)

data UnionVal = UnionVal
  { unionValIdent    :: !Ident
  , unionValTableRef :: !TypeRef
  } deriving (Show, Eq)
