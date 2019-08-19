{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlatBuffers.Internal.Util where

import           Data.Bits          ( (.&.), Bits )
import           Data.List.NonEmpty ( NonEmpty(..) )

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

-- | Proof that a number is strictly positive.
newtype Positive a = Positive { getPositive :: a }
  deriving newtype (Eq, Show)

{-# INLINE positive #-}
positive :: (Num a, Ord a) => a -> Maybe (Positive a)
positive n = if n > 0 then Just (Positive n) else Nothing
