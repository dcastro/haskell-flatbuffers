module Main where

import WeighBench.Encode (groups)
import           Weigh

main :: IO ()
main =
  mainWith $
    groups