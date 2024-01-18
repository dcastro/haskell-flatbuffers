module Types where

import           FlatBuffers

$(mkFlatBuffers "bench/types.fbs" defaultOptions)
