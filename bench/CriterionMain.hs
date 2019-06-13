module Main where

import           CriterionBench.Encode ( groups )

import           Criterion.Main

main = defaultMain groups
