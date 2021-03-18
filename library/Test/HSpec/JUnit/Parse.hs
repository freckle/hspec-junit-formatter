module Test.HSpec.JUnit.Parse
  ( parseJUnit
  , denormalize
  ) where

import Prelude

import Control.Monad.Catch (MonadThrow)
import Data.Conduit (ConduitT, awaitForever, yield)
import Data.XML.Types (Event)
import Test.HSpec.JUnit.Schema
import Text.XML.Stream.Parse
  (choose, content, many, requireAttr, tag', tagNoAttr)

denormalize' :: Suite -> [Suite]
denormalize' (Suite name xs) = collapse $ concatMap suiteOrCase xs
 where
  suiteOrCase = \case
    Right x -> [Suite name [Right x]]
    Left (Suite name' ys) -> denormalize' $ Suite (name <> "/" <> name') ys

collapse :: [Suite] -> [Suite]
collapse [] = []
collapse (x : y : xs)
  | suiteName x == suiteName y
  = collapse $ Suite (suiteName x) (suiteCases x <> suiteCases y) : xs
  | otherwise
  = x : collapse (y : xs)
collapse xs@(_ : _) = xs

-- | Denormalize nested <testsuite /> elements
--
-- HSpec's formatter cannot correctly output JUnit, so we must denormalize
-- nested <testsuite /> elements. Nested elements have their names collapsed
-- into `hspec` style paths.
--
denormalize :: MonadThrow m => ConduitT Suites Suites m ()
denormalize = awaitForever $ \(Suites name children) ->
  yield . Suites name $ concatMap denormalize' children

parseJUnit :: MonadThrow m => ConduitT Event Suites m ()
parseJUnit = maybe (pure ()) yield =<< parseSuite
 where
  parseSuite =
    tag' "testsuites" (requireAttr "name") $ \name -> Suites name <$> many suite

suite :: MonadThrow m => ConduitT Event o m (Maybe Suite)
suite = tag' "testsuite" (requireAttr "name") $ \name ->
  Suite name <$> many (choose [fmap Right <$> testCase, fmap Left <$> suite])

testCase :: MonadThrow m => ConduitT Event o m (Maybe TestCase)
testCase =
  tag' "testcase" ((,) <$> requireAttr "name" <*> requireAttr "classname")
    $ \(name, className) -> TestCase className name <$> result

result :: MonadThrow m => ConduitT Event o m (Maybe Result)
result = choose
  [ tag' "failure" (requireAttr "type") $ \fType -> Failure fType <$> content
  , tagNoAttr "skipped" $ Skipped <$> content
  ]
