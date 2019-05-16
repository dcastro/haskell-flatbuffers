module FlatBuffers.Internal.Compiler.NamingConventions where

import           Data.Text ( Text )
import qualified Data.Text as T

-- Style guide: https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html

dataTypeName :: Text -> Text
dataTypeName text =
  let (h, t) = T.splitAt 1 text
  in  T.toUpper h <> t

dataTypeConstructor :: Text -> Text
dataTypeConstructor = term

term :: Text -> Text
term text =
  let (h, t) = T.splitAt 1 text
  in  T.toLower h <> t

