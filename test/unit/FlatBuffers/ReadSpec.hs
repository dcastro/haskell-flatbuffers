{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.ReadSpec where

import           Control.Exception.Safe
import           Control.Monad.Except
import           Data.Functor.Identity
import           FlatBuffers
import           FlatBuffers.Read
import           Test.Hspec

spec :: Spec
spec =
  describe "read" $
  it "throws when buffer is exhausted" $
  fromLazyByteString "" `shouldThrow` \x ->
    x == ParsingError 0 "not enough bytes"
