module Test.Hspec.JUnit.Render
  ( renderJUnit
  ) where

import Prelude

import Conduit (ConduitT, awaitForever, mergeSource, yield, yieldMany, (.|))
import Control.Monad.Catch (MonadThrow)
import Data.Array qualified as Array
import Data.Conduit.List qualified as CL
import Data.Foldable (traverse_)
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Time.ISO8601 (formatISO8601)
import Data.XML.Types (Event)
import Test.Hspec.JUnit.Schema
  ( Location (..)
  , Result (..)
  , Suite (..)
  , Suites (..)
  , TestCase (..)
  )
import Text.Printf
import Text.Regex.Base qualified as Regex
import Text.Regex.TDFA.Text qualified as Regex
import Text.XML.Stream.Render (attr, content, tag)

renderJUnit :: MonadThrow m => Bool -> ConduitT Suites Event m ()
renderJUnit shouldDropConsoleFormatting = awaitForever $ \s ->
  tag "testsuites" (attr "name" s.name)
    $ yieldMany s.suites
      .| mergeSource idStream
      .| suite shouldDropConsoleFormatting
 where
  idStream = CL.iterate (+ 1) 0

suite :: MonadThrow m => Bool -> ConduitT (Int, Suite) Event m ()
suite shouldDropConsoleFormatting = awaitForever $ \(i, s) ->
  tag "testsuite" (attributes i s) $ do
    tag "properties" mempty mempty
    yieldMany s.cases .| testCase shouldDropConsoleFormatting
 where
  -- TODO these need to be made real values
  attributes i s =
    attr "name" s.name
      <> attr "package" s.name
      <> attr "id" (tshow i)
      <> attr "time" (roundToStr $ sumDurations s.cases)
      <> attr "timestamp" (pack $ formatISO8601 s.timestamp)
      <> attr "hostname" "localhost"
      <> attr "tests" (tshow $ length s.cases)
      <> attr "failures" (tshow $ length [() | Just Failure {} <- (.result) <$> s.cases])
      <> attr "errors" "0"
      <> attr "skipped" (tshow $ length [() | Just Skipped {} <- (.result) <$> s.cases])

tshow :: Show a => a -> Text
tshow = pack . show

testCase :: MonadThrow m => Bool -> ConduitT TestCase Event m ()
testCase shouldDropConsoleFormatting =
  awaitForever $ \(TestCase mLocation className name duration mResult) ->
    tag "testcase" (attributes mLocation className name duration)
      $ traverse_ yield mResult
        .| result shouldDropConsoleFormatting
 where
  attributes mLocation className name duration =
    maybe mempty (attr "file" . pack . (.file)) mLocation
      <> maybe mempty (attr "line" . pack . show . (.line)) mLocation
      <> attr "name" name
      <> attr "classname" className
      <> attr "time" (roundToStr duration)

result :: MonadThrow m => Bool -> ConduitT Result Event m ()
result shouldDropConsoleFormatting = awaitForever go
 where
  go (Failure fType contents) =
    tag "failure" (attr "type" fType) $ content $ dropConsoleFormatting contents
  go (Skipped contents) = tag "skipped" mempty $ content $ dropConsoleFormatting contents

  -- Drops ANSI control characters which might be used to set colors.
  -- Including these breaks XML, there is not much point encoding them.
  dropConsoleFormatting :: Text -> Text
  dropConsoleFormatting input
    | shouldDropConsoleFormatting =
        let
          regex = Regex.makeRegex (pack "\x1b\\[[0-9;]*[mGKHF]") :: Regex.Regex
          matches = Regex.matchAll regex input
          dropMatch (offset, len) input' =
            let
              (begining, rest) = Text.splitAt offset input'
              (_, end) = Text.splitAt len rest
            in
              begining <> end
          matchTuples = map (Array.! 0) matches
        in
          foldr dropMatch input matchTuples
    | otherwise = input

sumDurations :: [TestCase] -> Double
sumDurations cases = sum $ (.duration) <$> cases

roundToStr :: PrintfArg a => a -> Text
roundToStr = pack . printf "%0.9f"
