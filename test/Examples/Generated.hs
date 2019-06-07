{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.Generated where

import           FlatBuffers.Internal.Compiler.TH

$(mkFlatBuffers "test/Examples/schema.fbs"           defaultOptions)
$(mkFlatBuffers "test/Examples/vector_of_unions.fbs" defaultOptions)
