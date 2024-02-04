module Main where

import Compare
import Criterion.Main
import DecodeVectors
import Encode
import EncodeVectors

main :: IO ()
main =
  defaultMain $
    mconcat
    -- [ Encode.groups
    -- , EncodeVectors.groups
    -- , DecodeVectors.groups
    [ Compare.groups
    ]
