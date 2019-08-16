{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module CriterionBench.DecodeVectors where

import           Control.Monad

import           Criterion

import qualified Data.ByteString.Lazy       as BSL
import           Data.Functor               ( (<&>) )
import           Data.Int
import qualified Data.List                  as L
import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import           Data.Word

import           Debug.Trace

import           FlatBuffers.Internal.Read
import           FlatBuffers.Internal.Write

import           Types

groups :: [Benchmark]
groups =
  [ bgroup "decode vectors"
    [ bgroup "toList"
        [ bench "bool" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsK
        , bench "word8" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsA
        , bench "word16" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsB
        , bench "word32" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsC
        , bench "word64" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsD
        , bench "int8"  $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsE
        , bench "int16" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsF
        , bench "int32" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsG
        , bench "int64" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsH
        , bench "float" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsI
        , bench "double" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsJ
        , bench "string" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsL
        ]
    , bgroup "index"
        [ bench "word8" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i)
            )
            $ vectorsTable >>= vectorsA

        , bench "int32" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i)
            )
            $ vectorsTable >>= vectorsG

        , bench "string" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i)
            )
            $ vectorsTable >>= vectorsL
        ]
    ]
  ]

n :: Num a => a
n = 10000

mkNumList :: Num a => Int32 -> [a]
mkNumList len = fromIntegral <$> [1 .. len]

mkNumVec :: (Num a, WriteVectorElement a) => Maybe (WriteVector a)
mkNumVec = Just (vector n (mkNumList n))

vectorsTable :: Either ReadError (Table Vectors)
vectorsTable =
  decode . encode $
    vectors
      mkNumVec mkNumVec mkNumVec mkNumVec
      mkNumVec mkNumVec mkNumVec mkNumVec
      mkNumVec mkNumVec
      (Just . vector n . L.replicate n $ True)
      (Just . vector n $ [1..n] <&> \i -> T.take (i `rem` 15) "abcghjkel;jhgx")
      Nothing
      Nothing
      Nothing
      Nothing


