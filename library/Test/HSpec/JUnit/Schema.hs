module Test.HSpec.JUnit.Schema
  ( Suites(..)
  , Suite(..)
  , TestCase(..)
  , Result(..)
  ) where

import Prelude

import Data.Text (Text)
import Data.Time (UTCTime)

data Suites = Suites
  { suitesName :: Text
  , suitesSuites :: [Suite]
  }
  deriving stock Show

data Suite = Suite
  { suiteName :: Text
  , suiteTimestamp :: UTCTime
  , suiteCases :: [TestCase]
  }
  deriving stock Show

data TestCase = TestCase
  { testCaseClassName :: Text
  , testCaseName :: Text
  , testCaseDuration :: Double
  , testCaseResult :: Maybe Result
  }
  deriving stock Show

data Result = Failure Text Text | Skipped Text
  deriving stock Show
