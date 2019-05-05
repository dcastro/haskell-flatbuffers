module TestUtils where

import           Test.HUnit ( assertFailure )
import           Test.Hspec


-- | Useful when there's no `Show`/`Eq` instances for @a@.
shouldBeLeft :: Show e => Eq e => Either e a -> e -> Expectation
shouldBeLeft ea expected = case ea of
    Left e  -> e `shouldBe` expected
    Right _ -> expectationFailure "Expected 'Left', got 'Right'"

shouldBeRightAnd :: Either e a -> (a -> Bool) -> Expectation
shouldBeRightAnd ea pred = case ea of
    Left _  -> expectationFailure "Expected 'Right', got 'Left'"
    Right a -> pred a `shouldBe` True

shouldBeRightAndExpect :: Either e a -> (a -> Expectation) -> Expectation
shouldBeRightAndExpect ea expect = case ea of
    Left _  -> expectationFailure "Expected 'Right', got 'Left'"
    Right a -> expect a

fromRight :: Either e a -> IO a
fromRight ea = case ea of
    Left _  -> expectationFailure' "Expected 'Right', got 'Left'"
    Right a -> pure a

-- | Like `expectationFailure`, but returns @IO a@ instead of @IO ()@.
expectationFailure' :: HasCallStack => String -> IO a
expectationFailure' = Test.HUnit.assertFailure


