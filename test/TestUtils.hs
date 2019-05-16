{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestUtils where

import           Control.Monad             ( (>=>) )

import qualified Data.Aeson                as J
import           Data.Aeson.Encode.Pretty  ( encodePretty )
import qualified Data.ByteString.Lazy.UTF8 as BSLU

import qualified Data.Text.Lazy            as TL

import           Test.HUnit                ( assertFailure )
import           Test.Hspec

import qualified Text.Pretty.Simple        as PP


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

fromRight :: HasCallStack => Show e => Either e a -> IO a
fromRight ea = case ea of
    Left e  -> expectationFailure' $ "Expected 'Right', got 'Left':\n" <> show e
    Right a -> pure a

fromJust :: HasCallStack => Maybe a -> IO a
fromJust mb = case mb of
  Nothing -> expectationFailure' $ "Expected 'Just', got 'Nothing'"
  Just a  -> pure a

fromRightJust :: HasCallStack => Show e => Either e (Maybe a) -> IO a
fromRightJust = fromRight >=> fromJust

-- | Like `expectationFailure`, but returns @IO a@ instead of @IO ()@.
expectationFailure' :: HasCallStack => String -> IO a
expectationFailure' = Test.HUnit.assertFailure


-- | Pretty-prints objects when a test fails.
newtype Pretty a = Pretty a
  deriving newtype Eq

instance Show a => Show (Pretty a) where
  show (Pretty a) = TL.unpack (PP.pShowOpt opt a)
    where
      opt = PP.defaultOutputOptionsNoColor { PP.outputOptionsIndentAmount = 2 }

pshouldBe :: (Show a, Eq a, HasCallStack) => a -> a -> Expectation
pshouldBe x y = Pretty x `shouldBe` Pretty y


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

