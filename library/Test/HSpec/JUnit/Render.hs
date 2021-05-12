module Test.HSpec.JUnit.Render
  ( renderJUnit
  ) where

import Prelude

import Control.Monad.Catch (MonadThrow)
import Data.Conduit (ConduitT, awaitForever, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.Foldable (traverse_)
import Data.Hashable (hash)
import Data.Text (Text, pack)
import Data.XML.Types (Event)
import Test.HSpec.JUnit.Schema (Result(..), Suite(..), Suites(..), TestCase(..))
import Text.XML.Stream.Render (attr, content, tag)

renderJUnit :: MonadThrow m => ConduitT Suites Event m ()
renderJUnit = awaitForever $ \(Suites name suites) ->
  tag "testsuites" (attr "name" name) $ CL.sourceList suites .| suite

suite :: MonadThrow m => ConduitT Suite Event m ()
suite = awaitForever $ \(Suite name cases) ->
  tag "testsuite" (attributes name cases) $ do
    tag "properties" mempty mempty
    CL.sourceList cases .| do
      awaitForever $ \x -> yield x .| testCase
 where
  -- TODO these need to be made real values
  attributes name cases =
    attr "name" name
      <> attr "package" name
      <> attr "id" (tshow $ hash name)
      <> attr "time" (tshow $ sumDurations cases)
      <> attr "timestamp" "1979-01-01T01:01:01"
      <> attr "hostname" "localhost"
      <> attr "tests" (tshow $ length cases)
      <> attr
           "failures"
           (tshow $ length [ () | Just Failure{} <- testCaseResult <$> cases ])
      <> attr "errors" "0"
      <> attr
           "skipped"
           (tshow $ length [ () | Just Skipped{} <- testCaseResult <$> cases ])

tshow :: Show a => a -> Text
tshow = pack . show

testCase :: MonadThrow m => ConduitT TestCase Event m ()
testCase = awaitForever $ \(TestCase className name duration mResult) ->
  tag "testcase" (attributes className name duration)
    $ traverse_ yield mResult
    .| result
 where
  -- TODO these need to be made real values
  attributes className name duration =
    attr "name" name <> attr "classname" className <> attr
      "time"
      (tshow duration)

result :: MonadThrow m => ConduitT Result Event m ()
result = awaitForever go
 where
  go (Failure fType contents) =
    tag "failure" (attr "type" fType) $ content contents
  go (Skipped contents) = tag "skipped" mempty $ content contents

sumDurations :: [TestCase] -> Double
sumDurations cases = sum $ testCaseDuration <$> cases
