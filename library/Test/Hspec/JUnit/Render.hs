module Test.Hspec.JUnit.Render
  ( renderJUnit
  ) where

import Prelude

import Control.Monad.Catch (MonadThrow)
import Data.Conduit (ConduitT, awaitForever, mergeSource, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.Foldable (traverse_)
import Data.Text (Text, pack)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.XML.Types (Event)
import Test.Hspec.JUnit.Schema
  (Location(..), Result(..), Suite(..), Suites(..), TestCase(..))
import Text.Printf
import Text.XML.Stream.Render (attr, content, tag)

renderJUnit :: MonadThrow m => ConduitT Suites Event m ()
renderJUnit = awaitForever $ \Suites {..} ->
  tag "testsuites" (attr "name" suitesName)
    $ CL.sourceList suitesSuites
    .| mergeSource idStream
    .| suite
  where idStream = CL.iterate (+ 1) 0

suite :: MonadThrow m => ConduitT (Int, Suite) Event m ()
suite = awaitForever $ \(i, theSuite@Suite {..}) ->
  tag "testsuite" (attributes i theSuite) $ do
    tag "properties" mempty mempty
    CL.sourceList suiteCases .| do
      awaitForever $ \x -> yield x .| testCase
 where
  -- TODO these need to be made real values
  attributes i Suite {..} =
    attr "name" suiteName
      <> attr "package" suiteName
      <> attr "id" (tshow i)
      <> attr "time" (roundToStr $ sumDurations suiteCases)
      <> attr "timestamp" (pack $ iso8601Show suiteTimestamp)
      <> attr "hostname" "localhost"
      <> attr "tests" (tshow $ length suiteCases)
      <> attr
           "failures"
           (tshow
           $ length [ () | Just Failure{} <- testCaseResult <$> suiteCases ]
           )
      <> attr "errors" "0"
      <> attr
           "skipped"
           (tshow
           $ length [ () | Just Skipped{} <- testCaseResult <$> suiteCases ]
           )

tshow :: Show a => a -> Text
tshow = pack . show

testCase :: MonadThrow m => ConduitT TestCase Event m ()
testCase =
  awaitForever $ \(TestCase mLocation className name duration mResult) ->
    tag "testcase" (attributes mLocation className name duration)
      $ traverse_ yield mResult
      .| result
 where
  attributes mLocation className name duration =
    maybe mempty (attr "file" . pack . locationFile) mLocation
      <> maybe mempty (attr "line" . pack . show . locationLine) mLocation
      <> attr "name" name
      <> attr "classname" className
      <> attr "time" (roundToStr duration)

result :: MonadThrow m => ConduitT Result Event m ()
result = awaitForever go
 where
  go (Failure fType contents) =
    tag "failure" (attr "type" fType) $ content contents
  go (Skipped contents) = tag "skipped" mempty $ content contents

sumDurations :: [TestCase] -> Double
sumDurations cases = sum $ testCaseDuration <$> cases

roundToStr :: (PrintfArg a) => a -> Text
roundToStr = pack . printf "%0.9f"
