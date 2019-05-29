module FlatBuffers.Internal.Util where

import           Data.Bits          ( (.&.), Bits )
import           Data.List.NonEmpty ( NonEmpty(..), (<|) )
import           Data.Monoid        ( First(..) )

{-# INLINE headF #-}
headF :: Foldable f => f a -> Maybe a
headF xs = getFirst $ foldMap (First . Just) xs

{-# INLINE isPowerOfTwo #-}
isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = (n .&. (n - 1)) == 0

{-# INLINE roundUpToNearestMultipleOf #-}
roundUpToNearestMultipleOf :: Integral n => n -> n -> n
roundUpToNearestMultipleOf x y =
  case x `rem` y of
    0         -> x
    remainder -> (y - remainder) + x

{-# INLINE nonEmptyUnzip3 #-}
nonEmptyUnzip3 :: NonEmpty (a,b,c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
nonEmptyUnzip3 xs =
  ( (\(x, _, _) -> x) <$> xs
  , (\(_, x, _) -> x) <$> xs
  , (\(_, _, x) -> x) <$> xs
  )
