{-# LANGUAGE TemplateHaskell #-}

module Types where

import           FlatBuffers.Internal.Compiler.TH

$(mkFlatBuffers "bench/types.fbs" defaultOptions)
