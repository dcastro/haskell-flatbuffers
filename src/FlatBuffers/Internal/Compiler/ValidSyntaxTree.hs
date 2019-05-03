{-# LANGUAGE DerivingVia #-}

module FlatBuffers.Internal.Compiler.ValidSyntaxTree where

import           Data.Int
import           Data.List.NonEmpty                       ( NonEmpty )
import           Data.Scientific                          ( Scientific )
import           Data.String                              ( IsString(..) )
import           Data.Text                                ( Text )
import           Data.Word

import           FlatBuffers.Internal.Compiler.SyntaxTree ( HasIdent(..), Ident, Namespace, TypeRef )
import           FlatBuffers.Types

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
  { enumIdent     :: !Ident
  , enumType      :: !EnumType
  , enumVals      :: !(NonEmpty EnumVal)
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
  { structIdent      :: !Ident
  , structAlignment  :: !Alignment
  , structFields     :: !(NonEmpty StructField)
  } deriving (Show, Eq)

data StructField = StructField
  { structFieldIdent    :: !Ident
  , structFieldPadding  :: !Word8
  , structFieldType     :: !StructFieldType
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
  deriving (Eq, Show, Num, IsString, Fractional) via a

data Required = Req | Opt
  deriving (Eq, Show)

data IsRoot
  = NotRoot              -- ^ This table is not the root table.
  | IsRoot !(Maybe Text) -- ^ This table is the root table, and has an optional file identifier.
  deriving (Eq, Show)

data TableDecl = TableDecl
  { tableIdent     :: !Ident
  , tableIsRoot    :: !IsRoot
  , tableFields    :: ![TableField]
  } deriving (Eq, Show)

data TableField = TableField
  { tableFieldIdent      :: !Ident
  , tableFieldType       :: !TableFieldType
  , tableFieldDeprecated :: !Bool
  } deriving (Eq, Show)

data TableFieldType
  = TInt8   !(DefaultVal Int8)
  | TInt16  !(DefaultVal Int16)
  | TInt32  !(DefaultVal Int32)
  | TInt64  !(DefaultVal Int64)
  | TWord8  !(DefaultVal Word8)
  | TWord16 !(DefaultVal Word16)
  | TWord32 !(DefaultVal Word32)
  | TWord64 !(DefaultVal Word64)
  | TFloat  !(DefaultVal Scientific)
  | TDouble !(DefaultVal Scientific)
  | TBool   !(DefaultVal Bool)
  | TString !Required
  | TEnum   !TypeRef !(DefaultVal Integer)
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
  | VEnum   !TypeRef !InlineSize
  | VStruct !TypeRef !InlineSize
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
