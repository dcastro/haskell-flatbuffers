module Main where

import           CriterionBench.DecodeVectors
import           CriterionBench.Encode
import           CriterionBench.EncodeVectors
import           Criterion.Main

main =
  defaultMain $
    mconcat
    [ CriterionBench.Encode.groups
    , CriterionBench.EncodeVectors.groups
    , CriterionBench.DecodeVectors.groups
    ]
