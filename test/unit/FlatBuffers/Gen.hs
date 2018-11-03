{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module FlatBuffers.Gen where

import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.WithShow        (WithShow (WS), label, labelT, tshow,
                                       wsmap, wssequence)
import           FlatBuffers          (Field, InlineField, missing)
import qualified FlatBuffers          as F
import           Hedgehog
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

word8 = WS <$> label "word8 " <*> F.word8 <$> G.word8 R.constantBounded
word16 = WS <$> label "word16 " <*> F.word16 <$> G.word16 R.constantBounded
word32 = WS <$> label "word32 " <*> F.word32 <$> G.word32 R.constantBounded
word64 = WS <$> label "word64 " <*> F.word64 <$> G.word64 R.constantBounded

int8 = WS <$> label "int8 " <*> F.int8 <$> G.int8 R.constantBounded
int16 = WS <$> label "int16 " <*> F.int16 <$> G.int16 R.constantBounded
int32 = WS <$> label "int32 " <*> F.int32 <$> G.int32 R.constantBounded
int64 = WS <$> label "int64 " <*> F.int64 <$> G.int64 R.constantBounded

double = WS <$> label "double " <*> F.double <$> G.double (R.linearFrac (-10000000) 10000000)
float = WS <$> label "float " <*> F.float <$> G.float (R.linearFrac (-10000000) 10000000)

bool = WS <$> label "bool " <*> F.bool <$> G.bool

string = WS <$> label "string " <*> F.string <$> G.string textRange char
text = WS <$> label "text " <*> F.text <$> G.text textRange char
lazyText = WS <$> label "lazyText " <*> F.lazyText . TL.fromStrict <$> G.text textRange char
byteString = WS <$> label "byteString " <*> F.byteString <$> G.utf8 textRange char
lazyByteString = WS <$> label "lazyByteString " <*> F.lazyByteString . BSL.fromStrict <$> G.utf8 textRange char

scalar :: Gen (WithShow InlineField) -> Gen (WithShow Field)
scalar field = wsmap (T.append "scalar ") F.scalar' <$> field

textualField :: Gen (WithShow Field)
textualField =
  G.choice
    [ string
    , text
    , lazyText
    , byteString
    , lazyByteString
    ]

numericField :: Gen (WithShow InlineField)
numericField =
  G.choice
    [word8, word16, word32, word64, int8, int16, int32, int64, double, float]

field :: Gen (WithShow Field)
field =
  G.recursive G.choice
    [ pure $ WS "missing" missing
    , textualField
    , scalar numericField
    , scalar bool
    ]
    [ table
    , vector
    ]

table :: Gen (WithShow Field)
table =
  wsmap (labelT "table ") F.table . wssequence <$> G.list (R.linear 0 4) field

vector :: Gen (WithShow Field)
vector = do
  gen <- G.element $
    fmap scalar [word8, word16, word32, word64, int8, int16, int32, int64, double, float, bool]
    ++ [textualField, table]

  elems <- G.list (R.linear 0 10) gen
  pure $ wsmap (labelT "vector ") F.vector $ wssequence elems

-- | Generates a series of fields (which may contain nested tables, or vectors).
-- The field @f@ is guaranteed to be present somewhere in the structure, at any level of nesting.
-- If the field @f@ is present in a vector, then @gen@ is used to generate similar fields.
fieldsWith :: WithShow Field -> Gen (WithShow Field) -> Gen (WithShow [Field])
fieldsWith f gen =
  G.recursive
    G.choice
    [ do
        before <- G.list (R.linear 0 6) field
        after <- G.list (R.linear 0 6) field
        pure $ wssequence $ before ++ [f] ++ after
    , do
        before <- G.list (R.linear 0 6) field
        v <- vectorWith f gen
        after <- G.list (R.linear 0 6) field
        pure $ wssequence $ before ++ [v] ++ after
    ]
    [ do
        before <- G.list (R.linear 0 6) field
        t <- wsmap (labelT "table ") F.table <$> fieldsWith f gen
        after <- G.list (R.linear 0 6) field
        pure $ wssequence $ before ++ [t] ++ after
    ]


vectorWith :: WithShow Field -> Gen (WithShow Field) -> Gen (WithShow Field)
vectorWith f gen = do
  before <- G.list (R.linear 0 10) gen
  after <- G.list (R.linear 0 10) gen
  pure $ wsmap (labelT "vector ") F.vector $ wssequence (before ++ [f] ++ after)

char :: Gen Char
char = G.frequency [(9, G.alphaNum), (1, G.unicodeAll)]

textRange :: R.Range Int
textRange = R.linear 0 20
