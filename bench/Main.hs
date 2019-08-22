module Main where

import           DecodeVectors
import           Encode
import           EncodeVectors
import           Criterion.Main

main =
  defaultMain $
    mconcat
    [ Encode.groups
    , EncodeVectors.groups
    , DecodeVectors.groups
    ]
