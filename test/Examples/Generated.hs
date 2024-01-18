module Examples.Generated where

import           FlatBuffers (defaultOptions, mkFlatBuffers)

$(mkFlatBuffers "test/Examples/schema.fbs"           defaultOptions)
