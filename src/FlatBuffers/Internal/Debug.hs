module FlatBuffers.Internal.Debug
  ( showBuffer
  , showLazyBuffer
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List            (intercalate)

showBuffer :: BS.ByteString -> String
showBuffer bs =
  intercalate "\n" . fmap (intercalate ", ") . groupsOf 4 . fmap show $
  BS.unpack bs

showLazyBuffer :: BSL.ByteString -> String
showLazyBuffer bs =
  intercalate "\n" . fmap (intercalate ", ") . groupsOf 4 . fmap show $
  BSL.unpack bs

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs =
  case take n xs of
    [] -> []
    group -> group : groupsOf n (drop n xs)

