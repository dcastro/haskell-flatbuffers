{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module FlatBuffers.Internal.Compiler.NamingConventions where

import           Data.Text                                     ( Text )
import qualified Data.Text                                     as T

import           FlatBuffers.Internal.Compiler.ValidSyntaxTree ( HasIdent(..), Ident(..), Namespace(..), TypeRef(..), UnionDecl )

-- Style guide: https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html

typ :: Text -> Text
typ text =
  let (h, t) = T.splitAt 1 text
  in  T.toUpper h <> t

term :: Text -> Text
term text =
  let (h, t) = T.splitAt 1 text
  in  T.toLower h <> t

dataTypeConstructor :: Text -> Text
dataTypeConstructor = term

dataTypeName :: Text -> Text
dataTypeName = typ

modul :: Text -> Text
modul = typ

namespace :: Namespace -> Text
namespace (Namespace fragments) = T.intercalate "." (modul <$> fragments)

getter :: (HasIdent parent, HasIdent field) => parent -> field -> Text
getter (getIdent -> unIdent -> parent) (getIdent -> unIdent -> field) = term parent <> typ field

enumUnionMember :: (HasIdent parent, HasIdent val) => parent -> val -> Text
enumUnionMember (getIdent -> unIdent -> parentIdent) (getIdent -> unIdent -> valIdent) =
  typ parentIdent <> typ valIdent

unionClass :: UnionDecl -> Text
unionClass (getIdent -> unIdent -> unionIdent) =
  "Write" <> typ unionIdent

unionReadFun :: HasIdent union => union -> Text
unionReadFun (getIdent -> unIdent -> unionIdent) =
  "read" <> typ unionIdent

withModulePrefix :: Namespace -> Text -> Text
withModulePrefix ns text =
  if ns == ""
    then text
    else namespace ns <> "." <> text

