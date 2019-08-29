{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module DecodeVectors where

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

n :: Num a => a
n = 10000

groups :: [Benchmark]
groups =
  [ bgroup "decode vectors"
    [ bgroup "toList"
        [ bench "word8" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsA
        , bench "word16" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsB
        , bench "word32" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsC
        , bench "word64" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsD
        , bench "int8"  $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsE
        , bench "int16" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsF
        , bench "int32" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsG
        , bench "int64" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsH
        , bench "float" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsI
        , bench "double" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsJ
        , bench "bool" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsK
        , bench "string" $ nf (\(Right (Just vec)) -> toList vec ) $ vectorsTable >>= vectorsL
        , bench "struct" $ nf (\(Right (Just vec)) -> toList vec >>= traverse structWithOneIntX) $ vectorsTable >>= vectorsM
        , bench "table" $ nf (\(Right (Just vec)) -> toList vec >>= traverse pairTableX) $ vectorsTable >>= vectorsN
        , bench "union" $ nf (\(Right (Just vec)) -> do
            list <- toList vec
            forM list $ \case
              Union (WeaponUnionSword sword) -> swordTableX sword
              Union (WeaponUnionAxe axe)     -> axeTableX axe
          ) $ vectorsTable >>= vectorsO
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
      (Just . vector n . fmap structWithOneInt $ mkNumList n)
      (Just . vector n . fmap (\i -> pairTable (Just i) (Just i)) $ mkNumList n)
      (Just . vector n . fmap mkUnion $ mkNumList n
      )
  where
    mkUnion i =
      if odd i
        then weaponUnionSword (swordTable (Just i))
        else weaponUnionAxe  (axeTable (Just i))


