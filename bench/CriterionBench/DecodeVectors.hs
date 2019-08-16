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
        [ bench "bool" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Bool]) $ vectorsTable >>= vectorsK
        , bench "word8" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Word8]) $ vectorsTable >>= vectorsA
        , bench "word16" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Word16]) $ vectorsTable >>= vectorsB
        , bench "word32" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Word32]) $ vectorsTable >>= vectorsC
        , bench "word64" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Word64]) $ vectorsTable >>= vectorsD
        , bench "int8"  $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Int8]) $ vectorsTable >>= vectorsE
        , bench "int16" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Int16]) $ vectorsTable >>= vectorsF
        , bench "int32" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Int32]) $ vectorsTable >>= vectorsG
        , bench "int64" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Int64]) $ vectorsTable >>= vectorsH
        , bench "float" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Float]) $ vectorsTable >>= vectorsI
        , bench "double" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Double]) $ vectorsTable >>= vectorsJ
        , bench "string" $ nf (\(Right (Just vec)) -> toList vec :: Either ReadError [Text]) $ vectorsTable >>= vectorsL
        ]
    , bgroup "index"
        [ bench "word8" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i) :: Either ReadError [Word8]
            )
            $ vectorsTable >>= vectorsA

        , bench "int32" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i) :: Either ReadError [Int32]
            )
            $ vectorsTable >>= vectorsG

        , bench "string" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i) :: Either ReadError [Text]
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


