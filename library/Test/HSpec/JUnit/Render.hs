module Test.HSpec.JUnit.Render
  ( renderJUnit
  ) where

import Prelude

import Control.Monad.Catch (MonadThrow)
import Data.Conduit (ConduitT, awaitForever, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.Foldable (traverse_)
import Data.Text (Text, pack)
import Data.XML.Types (Event)
import Test.HSpec.JUnit.Schema (Result(..), TestCase(..), Suite(..), Suites(..))
import Text.XML.Stream.Render (attr, content, tag)

renderJUnit :: MonadThrow m => ConduitT Suites Event m ()
renderJUnit = awaitForever $ \(Suites name suites) ->
  tag "testsuites" (attr "name" name) $ CL.sourceList suites .| suite

suite :: MonadThrow m => ConduitT Suite Event m ()
suite =
  awaitForever
    $ \(Suite name cases) -> tag "testsuite" (attributes name cases) $ do
        tag "properties" mempty mempty
        CL.sourceList cases .| do
          awaitForever $ \case
            Left x -> yield x .| suite
            Right x -> yield x .| testCase
 where
  -- TODO these need to be made real values
  attributes name cases =
    attr "name" name
      <> attr "package" name
      <> attr "id" "0"
      <> attr "time" "0"
      <> attr "timestamp" "1979-01-01T01:01:01"
      <> attr "hostname" "localhost"
      <> attr "tests" (tshow $ length cases)
      <> attr
           "failures"
           (tshow $ length
             [ () | Right (TestCase _ (Just (Failure _ _))) <- cases ]
           )
      <> attr "errors" "0"
      <> attr
           "skipped"
           (tshow
           $ length [ () | Right (TestCase _ (Just (Skipped _))) <- cases ]
           )

tshow :: Show a => a -> Text
tshow = pack . show

testCase :: MonadThrow m => ConduitT TestCase Event m ()
testCase = awaitForever $ \(TestCase name mResult) ->
  tag "testcase" (attributes name) $ traverse_ yield mResult .| result
 where
  nameAttr name = attr "name" name <> attr "classname" name
  -- TODO these need to be made real values
  attributes name = nameAttr name <> attr "time" "0"

result :: MonadThrow m => ConduitT Result Event m ()
result = awaitForever go
 where
  go (Failure fType contents) =
    tag "failure" (attr "type" fType) $ content contents
  go (Skipped contents) = tag "skipped" mempty $ content contents
