-- | This module is intended to be imported qualified to avoid name clashes with Prelude.
-- E.g.:
--
-- > import           FlatBuffers.Vector (Vector, WriteVector)
-- > import qualified FlatBuffers.Vector as Vector
module FlatBuffers.Vector
  (
    -- * Creating a vector
    W.WriteVectorElement(..)
  , W.fromMonoFoldable'
  , W.fromList
  , W.fromList'
  , W.singleton
  , W.empty

    -- * Reading a vector
  , R.VectorElement(..)
  , R.index
  ) where

import           FlatBuffers.Internal.Read  as R
import           FlatBuffers.Internal.Write as W



