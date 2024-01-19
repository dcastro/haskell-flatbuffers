module TestImports
  ( module Hspec
  , module Hedgehog
  , shouldBeLeft
  , shouldBeRightAnd
  , shouldBeRightAndExpect
  , evalRight
  , evalJust
  , evalRightJust
  , liftA4
  , PrettyJson(..)
  , shouldBeJson
  , showBuffer
  , traceBufferM
  , showBufferHex
  , traceBufferHexM
  ) where

import Control.Monad ((>=>))
import Data.Aeson qualified as J
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.UTF8 qualified as BSLU
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Text qualified as T
import Debug.Trace
import Hedgehog
import Test.Hspec.Core.Hooks as Hspec
import Test.Hspec.Core.Spec as Hspec
import Test.Hspec.Expectations.Pretty as Hspec hiding (Expectation)
import Test.Hspec.Hedgehog as Hedgehog
import Test.Hspec.Runner as Hspec
import Test.HUnit (assertFailure)
import Text.Hex

-- | Useful when there's no `Show`/`Eq` instances for @a@.
shouldBeLeft :: HasCallStack => Show e => Eq e => Either e a -> e -> Expectation
shouldBeLeft ea expected = case ea of
    Left e  -> e `shouldBe` expected
    Right _ -> expectationFailure "Expected 'Left', got 'Right'"

shouldBeRightAnd :: HasCallStack => Show e => Either e a -> (a -> Bool) -> Expectation
shouldBeRightAnd ea pred = case ea of
    Left e  -> expectationFailure $ "Expected 'Right', got 'Left':\n" <> show e
    Right a -> pred a `shouldBe` True

shouldBeRightAndExpect :: HasCallStack => Show e => Either e a -> (a -> Expectation) -> Expectation
shouldBeRightAndExpect ea expect = case ea of
    Left e  -> expectationFailure $ "Expected 'Right', got 'Left':\n" <> show e
    Right a -> expect a

evalRight :: HasCallStack => Show e => Either e a -> IO a
evalRight ea = case ea of
    Left e  -> expectationFailure' $ "Expected 'Right', got 'Left':\n" <> show e
    Right a -> pure a

evalJust :: HasCallStack => Maybe a -> IO a
evalJust mb = case mb of
  Nothing -> expectationFailure' "Expected 'Just', got 'Nothing'"
  Just a  -> pure a

evalRightJust :: HasCallStack => Show e => Either e (Maybe a) -> IO a
evalRightJust = evalRight >=> evalJust

-- | Like `expectationFailure`, but returns @IO a@ instead of @IO ()@.
expectationFailure' :: HasCallStack => String -> IO a
expectationFailure' = Test.HUnit.assertFailure

-- | Allows Json documents to be compared (using e.g. `shouldBe`) and pretty-printed in case the comparison fails.
newtype PrettyJson = PrettyJson J.Value
  deriving Eq

instance Show PrettyJson where
  show (PrettyJson v) = BSLU.toString (encodePretty v)

shouldBeJson :: HasCallStack => J.Value -> J.Value -> Expectation
shouldBeJson x y = PrettyJson x `shouldBe` PrettyJson y

liftA4 ::
   Applicative m =>
   (a -> b -> c -> d -> r) -> m a -> m b -> m c -> m d -> m r
liftA4 fn a b c d = fn <$> a <*> b <*> c <*> d


traceBufferM :: Applicative m => BSL.ByteString -> m ()
traceBufferM = traceM . showBuffer

showBuffer :: BSL.ByteString -> String
showBuffer bs =
  List.intercalate "\n" . fmap (List.intercalate ", ") . groupsOf 4 . fmap show $
  BSL.unpack bs

traceBufferHexM :: Applicative m => BSL.ByteString -> m ()
traceBufferHexM = traceM . showBufferHex

showBufferHex :: BSL.ByteString -> String
showBufferHex bs =
  bs
    & BSL.toStrict
    & encodeHex
    & T.unpack
    & groupsOf 2
    & groupsOf 4
    <&> List.intercalate ", "
    & List.intercalate "\n"

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs =
  case take n xs of
    []    -> []
    group -> group : groupsOf n (drop n xs)
