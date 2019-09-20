module Encode where

import           Criterion

import           FlatBuffers

import           Types


groups :: [Benchmark]
groups =
  [ bgroup "encode"
    [ bench "scalars" $ nf encode $
        scalars
            (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
            (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
            (Just 1234.56) (Just 2873242.82782) (Just True) $ Just $
          scalars
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just 1234.56) (Just 2873242.82782) (Just True) $ Just $
            scalars
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
              (Just 1234.56) (Just 2873242.82782) (Just True) Nothing
    ]
  ]


