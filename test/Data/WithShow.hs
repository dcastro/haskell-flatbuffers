{-# LANGUAGE OverloadedStrings #-}

module Data.WithShow where

import           Data.Text (Text)
import qualified Data.Text as T

-- | Wraps a non-showable value and gives it a show instance.
--
-- We use this to print `Field`, `InlineField`, etc. when a property test fails,
-- so it can easily be copy-pasted and reproduced.
data WithShow a = WS
  { wshow :: Text
  , value :: a
  }

instance Show (WithShow a) where
  show = show . wshow

wsmap :: (Text -> Text) -> (a -> b) -> (WithShow a -> WithShow b)
wsmap f g (WS x y) = WS (f x) (g y)

wssequence :: [WithShow a] -> WithShow [a]
wssequence xs =
  WS
    ("[ " <> T.intercalate ", " (map wshow xs) <> " ]")
    (map value xs)

tshow :: Show a => a -> Text
tshow = T.pack . show

labelT :: Text -> Text -> Text
labelT = T.append

label :: Show a => Text -> a -> Text
label l = T.append l . tshow
