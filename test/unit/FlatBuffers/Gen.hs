{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.Gen where

import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.WithShow        (WithShow (WS), tshow, wsmap, wssequence, label)
import           FlatBuffers          as F
import           Hedgehog
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

textualField :: Gen (WithShow Field)
textualField =
  G.choice
    [ WS <$> label "string " <*> string <$> G.string (R.linear 0 20) char
    , WS <$> label "text " <*> text <$> G.text (R.linear 0 20) char
    , WS <$> label "lazyText " <*> lazyText . TL.fromStrict <$> G.text (R.linear 0 20) char
    , WS <$> label "byteString " <*> byteString <$> G.utf8 (R.linear 0 20) char
    , WS <$> label "lazyByteString " <*> lazyByteString . BSL.fromStrict <$> G.utf8 (R.linear 0 20) char
    ]

unsignedField :: Gen (WithShow InlineField)
unsignedField =
  G.choice
    [ WS <$> label "word8 " <*> word8 <$> G.word8 R.constantBounded
    , WS <$> label "word16 " <*> word16 <$> G.word16 R.constantBounded
    , WS <$> label "word32 " <*> word32 <$> G.word32 R.constantBounded
    , WS <$> label "word64 " <*> word64 <$> G.word64 R.constantBounded
    ]

field :: Gen (WithShow Field)
field =
  G.choice
    [ pure $ WS "missing" missing
    , textualField
    , wsmap (T.append "scalar ") scalar' <$> unsignedField
    ]

tableWith :: WithShow Field -> Gen (WithShow [Field])
tableWith f = do
  before <- G.list (R.linear 0 20) field
  after <- G.list (R.linear 0 20) field
  pure $ wssequence $ before ++ [f] ++ after

char :: Gen Char
char = G.frequency [(9, G.alphaNum), (1, G.unicodeAll)]
