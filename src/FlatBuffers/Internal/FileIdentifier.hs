module FlatBuffers.Internal.FileIdentifier
  ( HasFileIdentifier(..)
  , FileIdentifier(unFileIdentifier)
  , fileIdentifier
  , fileIdentifier'
  , unsafeFileIdentifier
  , unsafeFileIdentifier'
  ) where


import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as T

import FlatBuffers.Internal.Constants (fileIdentifierSize)

-- | An identifier that's used to "mark" a buffer.
-- To add this marker to a buffer, use `FlatBuffers.encodeWithFileIdentifier`.
-- To check whether a buffer contains the marker before decoding it, use `FlatBuffers.checkFileIdentifier`.
--
-- For more information on file identifiers, see :
--
-- * The [library's docs](https://github.com/dcastro/haskell-flatbuffers#file-identifiers)
-- * Section "File identification and extension" of the [official docs](https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html)
newtype FileIdentifier = FileIdentifier { unFileIdentifier :: ByteString }
  deriving (Eq, Show)

-- | Encodes the input text as UTF-8 and returns a @Just FileIdentifier@ if it has exactly 4 bytes,
-- otherwise `Nothing`.
fileIdentifier :: Text -> Maybe FileIdentifier
fileIdentifier = fileIdentifier' . T.encodeUtf8

-- | Returns a @Just FileIdentifier@ if the input `ByteString` has exactly 4 bytes,
-- otherwise `Nothing`.
fileIdentifier' :: ByteString -> Maybe FileIdentifier
fileIdentifier' bs =
  if BS.length bs /= fileIdentifierSize
    then Nothing
    else Just (FileIdentifier bs)

-- | Constructs a new `FileIdentifier` without checking its length.
unsafeFileIdentifier :: Text -> FileIdentifier
unsafeFileIdentifier = unsafeFileIdentifier' . T.encodeUtf8

-- | Constructs a new `FileIdentifier` without checking its length.
unsafeFileIdentifier' :: ByteString -> FileIdentifier
unsafeFileIdentifier' = FileIdentifier

-- | Associates a type with a file identifier.
-- To create an association, declare a @root_type@ and @file_identifier@ in your schema.
--
-- > table Player {}
-- > root_type Player;
-- > file_identifier "PLYR";
class HasFileIdentifier a where
  getFileIdentifier :: FileIdentifier
