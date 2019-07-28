{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import           FlatBuffers.Internal.Compiler.TH
import           FlatBuffers.Internal.Write

$(mkFlatBuffers "bench/types.fbs" defaultOptions)
