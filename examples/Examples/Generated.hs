{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.Generated where

import           FlatBuffers.Internal.Compiler.TH

$(mkFlatBuffers "examples/Examples/schema.fbs"           defaultOptions)
$(mkFlatBuffers "examples/Examples/vector_of_unions.fbs" defaultOptions)
