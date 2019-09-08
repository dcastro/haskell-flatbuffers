{-# LANGUAGE TemplateHaskell #-}

module Examples.Generated where

import           FlatBuffers

$(mkFlatBuffers "test/Examples/schema.fbs"           defaultOptions)
$(mkFlatBuffers "test/Examples/vector_of_unions.fbs" defaultOptions)
