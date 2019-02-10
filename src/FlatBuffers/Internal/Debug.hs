module FlatBuffers.Internal.Debug
  ( showBuffer
  , printBuffer
  ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy   as BSL
import           Data.List              (intercalate)

printBuffer :: MonadIO m => BSL.ByteString -> m ()
printBuffer = liftIO . putStrLn . showBuffer

showBuffer :: BSL.ByteString -> String
showBuffer bs =
  intercalate "\n" . fmap (intercalate ", ") . groupsOf 4 . fmap show $
  BSL.unpack bs

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs =
  case take n xs of
    [] -> []
    group -> group : groupsOf n (drop n xs)

