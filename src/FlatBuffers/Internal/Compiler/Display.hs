{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.Internal.Compiler.Display where

import           Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import           Data.Text          ( Text )

-- | Maps a value of type @a@ into a string that can be displayed to the user.
-- move this to its own file
class Display a where
  display :: a -> Text

instance Display Text where
  display = id

instance Display a => Display (NonEmpty a) where
  display = display . NE.toList

instance Display a => Display [a] where
  display xs = T.intercalate ", " (fmap displayOne xs)
    where
      displayOne x = "'" <> display x <> "'"

instance Display Integer where
  display = displayFromShow

displayFromShow :: Show a => a -> Text
displayFromShow = T.pack . show
