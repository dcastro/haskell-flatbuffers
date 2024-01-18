{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Use >=>" -}

module DecodeVectors where

import Control.Monad

import Criterion

import Data.Functor ((<&>))
import Data.Int
import Data.List qualified as L
import Data.Text qualified as T

import FlatBuffers
import FlatBuffers.Vector (index, unsafeIndex)
import FlatBuffers.Vector qualified as Vec

import Types

n :: Num a => a
n = 10000

groups :: [Benchmark]
groups =
  [ bgroup ("decode vectors (" <> show @Int n <> " elements)")
    [ bgroup "toList"
        [ bench "word8" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsA
        , bench "word16" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsB
        , bench "word32" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsC
        , bench "word64" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsD
        , bench "int8"  $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsE
        , bench "int16" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsF
        , bench "int32" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsG
        , bench "int64" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsH
        , bench "float" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsI
        , bench "double" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsJ
        , bench "bool" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsK
        , bench "string" $ nf (\(Right (Just vec)) -> Vec.toList vec ) $ vectorsTable >>= vectorsL
        , bench "struct" $ nf (\(Right (Just vec)) -> Vec.toList vec >>= traverse structWithOneIntX) $ vectorsTable >>= vectorsM
        , bench "table" $ nf (\(Right (Just vec)) -> Vec.toList vec >>= traverse pairTableX) $ vectorsTable >>= vectorsN
        , bench "union" $ nf (\(Right (Just vec)) -> do
            list <- Vec.toList vec
            forM list $ \case
              Union (WeaponUnionSword sword) -> swordTableX sword
              Union (WeaponUnionAxe axe)     -> axeTableX axe
          ) $ vectorsTable >>= vectorsO
        ]
    , bgroup "unsafeIndex"
        [ bench "word8" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `unsafeIndex` i)
            )
            $ vectorsTable >>= vectorsA

        , bench "int32" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `unsafeIndex` i)
            )
            $ vectorsTable >>= vectorsG

        , bench "struct" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `unsafeIndex` i >>= structWithOneIntX)
            )
            $ vectorsTable >>= vectorsM

        , bench "string" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `unsafeIndex` i)
            )
            $ vectorsTable >>= vectorsL
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

        , bench "struct" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i >>= structWithOneIntX)
            )
            $ vectorsTable >>= vectorsM

        , bench "string" $ nf (\(Right (Just vec)) ->
              forM [0..(n-1)] (\i -> vec `index` i)
            )
            $ vectorsTable >>= vectorsL
        ]
    ]
  ]

mkNumList :: Num a => Int32 -> [a]
mkNumList len = fromIntegral <$> [1 .. len]

mkNumVec :: (Num a, Vec.WriteVectorElement a) => Maybe (Vec.WriteVector a)
mkNumVec = Just (Vec.fromList n (mkNumList n))

vectorsTable :: Either ReadError (Table Vectors)
vectorsTable =
  decode . encode $
    vectors
      mkNumVec mkNumVec mkNumVec mkNumVec
      mkNumVec mkNumVec mkNumVec mkNumVec
      mkNumVec mkNumVec
      (Just . Vec.fromList n . L.replicate n $ True)
      (Just . Vec.fromList n $ [1..n] <&> \i -> T.take (i `rem` 15) "abcghjkel;jhgx")
      (Just . Vec.fromList n . fmap structWithOneInt $ mkNumList n)
      (Just . Vec.fromList n . fmap (\i -> pairTable (Just i) (Just i)) $ mkNumList n)
      (Just . Vec.fromList n . fmap mkUnion $ mkNumList n
      )
  where
    mkUnion i =
      if odd i
        then weaponUnionSword (swordTable (Just i))
        else weaponUnionAxe  (axeTable (Just i))
