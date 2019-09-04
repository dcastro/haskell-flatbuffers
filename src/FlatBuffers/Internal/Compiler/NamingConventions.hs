{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module FlatBuffers.Internal.Compiler.NamingConventions where

import           Data.Text                                     ( Text )
import qualified Data.Text                                     as T
import qualified Data.Text.Manipulate                          as TM

import           FlatBuffers.Internal.Compiler.ValidSyntaxTree ( EnumDecl, HasIdent(..), Ident(..), Namespace(..), UnionDecl, UnionVal )

-- Style guide: https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html

dataTypeConstructor :: HasIdent a => a -> Text
dataTypeConstructor = TM.toCamel . unIdent . getIdent

arg :: HasIdent a => a -> Text
arg = TM.toCamel . unIdent . getIdent

dataTypeName :: HasIdent a => a -> Text
dataTypeName = TM.toPascal . unIdent . getIdent

namespace :: Namespace -> Text
namespace (Namespace fragments) = T.intercalate "." (TM.toPascal <$> fragments)

getter :: (HasIdent parent, HasIdent field) => parent -> field -> Text
getter (getIdent -> unIdent -> parent) (getIdent -> unIdent -> field) =
  TM.toCamel parent <> TM.toPascal field

toEnumFun :: EnumDecl -> Text
toEnumFun enum =
  "to" <> TM.toPascal (unIdent (getIdent enum))

fromEnumFun :: EnumDecl -> Text
fromEnumFun enum =
  "from" <> TM.toPascal (unIdent (getIdent enum))

enumUnionMember :: (HasIdent parent, HasIdent val) => parent -> val -> Text
enumUnionMember (getIdent -> unIdent -> parentIdent) (getIdent -> unIdent -> valIdent) =
  TM.toPascal parentIdent <> TM.toPascal valIdent

unionConstructor :: UnionDecl -> UnionVal -> Text
unionConstructor union unionVal =
  TM.toCamel (unIdent $ getIdent union) <> TM.toPascal (unIdent $ getIdent unionVal)

readUnionFun :: HasIdent union => union -> Text
readUnionFun (getIdent -> unIdent -> unionIdent) =
  "read" <> TM.toPascal unionIdent

withModulePrefix :: Namespace -> Text -> Text
withModulePrefix ns text =
  if ns == ""
    then text
    else namespace ns <> "." <> text

