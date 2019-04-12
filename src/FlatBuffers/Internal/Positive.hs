{-# LANGUAGE DerivingVia #-}

module FlatBuffers.Internal.Positive 
  ( Positive(getPositive)
  , positive
  ) where

newtype Positive a = Positive { getPositive :: a }
  deriving (Eq, Show) via a

positive :: (Num a, Ord a) => a -> Maybe (Positive a)
positive n = if n > 0 then Just (Positive n) else Nothing
