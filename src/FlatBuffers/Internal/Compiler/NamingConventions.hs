{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module FlatBuffers.Internal.Compiler.NamingConventions where

import qualified Data.Set                                      as Set
import           Data.Text                                     ( Text )
import qualified Data.Text                                     as T
import qualified Data.Text.Manipulate                          as TM

import           FlatBuffers.Internal.Compiler.ValidSyntaxTree ( EnumDecl, EnumVal, HasIdent(..), Ident(..), Namespace(..), UnionDecl, UnionVal )

-- Style guide: https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html

dataTypeConstructor :: HasIdent a => a -> Text
dataTypeConstructor = replaceKeyword . TM.toCamel . unIdent . getIdent

arg :: HasIdent a => a -> Text
arg = TM.toCamel . unIdent . getIdent

dataTypeName :: HasIdent a => a -> Text
dataTypeName = TM.toPascal . unIdent . getIdent

namespace :: Namespace -> Text
namespace (Namespace fragments) = T.intercalate "." (TM.toPascal <$> fragments)

getter :: (HasIdent parent, HasIdent field) => parent -> field -> Text
getter (getIdent -> Ident parent) (getIdent -> Ident field) =
  TM.toCamel parent <> TM.toPascal field

toEnumFun :: EnumDecl -> Text
toEnumFun enum =
  "to" <> TM.toPascal (unIdent (getIdent enum))

fromEnumFun :: EnumDecl -> Text
fromEnumFun enum =
  "from" <> TM.toPascal (unIdent (getIdent enum))

enumUnionMember :: (HasIdent parent, HasIdent val) => parent -> val -> Text
enumUnionMember (getIdent -> Ident parentIdent) (getIdent -> Ident valIdent) =
  TM.toPascal parentIdent <> TM.toPascal valIdent

enumBitFlagsConstant :: EnumDecl -> EnumVal -> Text
enumBitFlagsConstant (getIdent -> Ident enumIdent) (getIdent -> Ident enumValIdent) =
  TM.toCamel enumIdent <> TM.toPascal enumValIdent

enumBitFlagsAllFun :: EnumDecl -> Text
enumBitFlagsAllFun (getIdent -> Ident enumIdent) =
  "all" <> TM.toPascal enumIdent

enumBitFlagsNamesFun :: EnumDecl -> Text
enumBitFlagsNamesFun (getIdent -> Ident enumIdent) =
  TM.toCamel enumIdent <> "Names"

enumNameFun :: EnumDecl -> Text
enumNameFun (getIdent -> Ident enumIdent) =
  TM.toCamel enumIdent <> "Name"

unionConstructor :: UnionDecl -> UnionVal -> Text
unionConstructor (getIdent -> Ident unionIdent) (getIdent -> Ident unionValIdent) =
  TM.toCamel unionIdent <> TM.toPascal unionValIdent

readUnionFun :: HasIdent union => union -> Text
readUnionFun (getIdent -> Ident unionIdent) =
  "read" <> TM.toPascal unionIdent

withModulePrefix :: Namespace -> Text -> Text
withModulePrefix ns text =
  if ns == ""
    then text
    else namespace ns <> "." <> text

keywords :: Set.Set Text
keywords = Set.fromList
  [ "as" , "case", "class", "data", "default", "deriving", "do"
  , "else", "hiding", "if", "import", "in", "infix", "infixl"
  , "infixr", "instance", "let", "module", "newtype", "of", "qualified"
  , "then", "type", "where", "forall", "mdo", "family", "role"
  , "pattern", "static", "stock", "anyclass", "via", "group", "by"
  , "using", "foreign", "export", "label", "dynamic", "safe"
  , "interruptible", "unsafe", "stdcall", "ccall", "capi", "prim"
  , "javascript", "unit", "dependency", "signature", "rec", "proc"
  ]

replaceKeyword :: Text -> Text
replaceKeyword x
  | x `Set.member` keywords = x <> "_"
  | otherwise = x
