{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module EncodeVectors where

import           Criterion

import           Data.Foldable              as F
import           Data.Functor               ( (<&>) )
import           Data.Int
import qualified Data.List                  as L
import           Data.Text                  ( Text )
import qualified Data.Vector                as V

import           FlatBuffers.Internal.Write

import           Types

n :: Num a => a
n = 10000

groups :: [Benchmark]
groups =
  [ bgroup "encode vectors"
    [ bgroup "from list"
      [ bench "of ints" $ nf (\xs ->
          encode . vectorOfInts . Just . vector (fromIntegral (F.length xs)) $
            xs
        ) $ mkIntList n

      , bench "of ints (with fusion)" $ nf (\xs ->
          encode . vectorOfInts . Just . vector (fromIntegral (F.length xs)) $
            userId <$> xs
        ) $ mkUserList n

      , bench "of structs (1 int field)" $ nf (\xs ->
          encode . vectorOfStructWithOneInt . Just . vector (fromIntegral (F.length xs)) $
            structWithOneInt <$> xs
        ) $ mkIntList n

      , bench "of structs (2 ints fields)" $ nf (\xs ->
          encode . vectorOfPairs . Just . vector (fromIntegral (F.length xs)) $
            (\(User id age _) -> pair id age) <$> xs
        ) $ mkUserList n

      , bench "of short strings" $ nf (\xs ->
          encode . vectorOfStrings . Just . vector (fromIntegral (F.length xs)) $
            xs
        ) $ mkTextList n

      , bench "of short strings (with fusion)" $ nf (\xs ->
          encode . vectorOfStrings . Just . vector (fromIntegral (F.length xs)) $
            userName <$> xs
        ) $ mkUserList n

      , bench "of long strings" $ nf (\xs ->
          encode . vectorOfStrings . Just . vector (fromIntegral (F.length xs)) $
            xs
        ) $ mkLongTextList n

      , bench "of tables (2 int fields)" $ nf (\xs ->
          encode . vectorOfTables . Just . vector (fromIntegral (F.length xs)) $
            (\(User id age _) -> pairTable (Just id) (Just age)) <$> xs
        ) $ mkUserList n

      , bench "of tables (1 int field, 1 string field)" $ nf (\xs ->
          encode . vectorOfUsers . Just . vector (fromIntegral (F.length xs)) $
            (\(User id _ name) -> userTable (Just id) (Just name)) <$> xs
        ) $ mkUserList n

      , bench "of unions (1 int field each)" $ nf (\xs ->
          encode . vectorOfUnions . Just . vector (fromIntegral (F.length xs)) $
            xs <&> \case
              Sword x -> weaponUnionSword (swordTable (Just x))
              Axe x   -> weaponUnionAxe   (axeTable   (Just x))
        ) $ mkWeaponList n
      ]

    , bgroup "from vector"
      [ bench "of ints" $ nf (\xs ->
          encode . vectorOfInts . Just . vector (fromIntegral (F.length xs)) $
            xs
        ) $ mkIntVector n

      , bench "of ints (with fusion)" $ nf (\xs ->
          encode . vectorOfInts . Just . vector (fromIntegral (F.length xs)) $
            userId <$> xs
        ) $ mkUserVector n

      , bench "of structs (1 int field)" $ nf (\xs ->
          encode . vectorOfStructWithOneInt . Just . vector (fromIntegral (F.length xs)) $
            structWithOneInt <$> xs
        ) $ mkIntVector n

      , bench "of structs (2 ints fields)" $ nf (\xs ->
          encode . vectorOfPairs . Just . vector (fromIntegral (F.length xs)) $
            (\(User id age _) -> pair id age) <$> xs
        ) $ mkUserVector n

      , bench "of short strings" $ nf (\xs ->
          encode . vectorOfStrings . Just . vector (fromIntegral (F.length xs)) $
            xs
        ) $ mkTextVector n

      , bench "of short strings (with fusion)" $ nf (\xs ->
          encode . vectorOfStrings . Just . vector (fromIntegral (F.length xs)) $
            userName <$> xs
        ) $ mkUserVector n

      , bench "of long strings" $ nf (\xs ->
          encode . vectorOfStrings . Just . vector (fromIntegral (F.length xs)) $
            xs
        ) $ mkLongTextVector n

      , bench "of tables (2 int fields)" $ nf (\xs ->
          encode . vectorOfTables . Just . vector (fromIntegral (F.length xs)) $
            (\(User id age _) -> pairTable (Just id) (Just age)) <$> xs
        ) $ mkUserVector n

      , bench "of tables (1 int field, 1 string field)" $ nf (\xs ->
          encode . vectorOfUsers . Just . vector (fromIntegral (F.length xs)) $
            (\(User id _ name) -> userTable (Just id) (Just name)) <$> xs
        ) $ mkUserVector n

      , bench "of unions (1 int field each)" $ nf (\xs ->
          encode . vectorOfUnions . Just . vector (fromIntegral (F.length xs)) $
            xs <&> \case
              Sword x -> weaponUnionSword (swordTable (Just x))
              Axe x   -> weaponUnionAxe   (axeTable   (Just x))
        ) $ mkWeaponVector n
      ]
    ]
  ]

data User = User
  { userId :: !Int32
  , userAge :: !Int32
  , userName :: !Text
  }

data Weapon
  = Sword !Int32
  | Axe !Int32

mkUserList :: Int32 -> [User]
mkUserList n = (\i -> User i (i+1) "abcdefghijk" ) <$> [1..n]

mkWeaponList :: Int32 -> [Weapon]
mkWeaponList n =
  [1..n] <&> \i ->
    if odd i
      then Sword i
      else Axe i

mkIntList :: Int32 -> [Int32]
mkIntList n = [1..n]

mkTextList :: Int -> [Text]
mkTextList n = L.replicate n "abcdefghijk"

mkLongTextList :: Int -> [Text]
mkLongTextList n = L.replicate n "abcghjkel;jhgxwflh;eokjclhukgwyfteci;owmnubyicvutywfygn;emo'pcnwhuegfcjkjkwelhgdfwgklked;lwjhkejvhjnwekndjkvwejhbjxknwejkvcxhwoipoqoyiugs"




mkUserVector :: Int32 -> V.Vector User
mkUserVector n = V.fromList (mkUserList n)

mkWeaponVector :: Int32 -> V.Vector Weapon
mkWeaponVector n = V.fromList (mkWeaponList n)

mkIntVector :: Int32 -> V.Vector Int32
mkIntVector n = V.fromList (mkIntList n)

mkTextVector :: Int -> V.Vector Text
mkTextVector n = V.fromList (mkTextList n)

mkLongTextVector :: Int -> V.Vector Text
mkLongTextVector n = V.fromList (mkLongTextList n)
