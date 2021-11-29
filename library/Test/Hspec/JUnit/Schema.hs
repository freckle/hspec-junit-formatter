module Test.Hspec.JUnit.Schema
  ( Suites(..)
  , Suite(..)
  , TestCase(..)
  , Location(..)
  , Result(..)
  ) where

import Prelude

import Data.Text (Text)
import Data.Time (UTCTime)
import Numeric.Natural

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
  { testCaseLocation :: Maybe Location
  , testCaseClassName :: Text
  , testCaseName :: Text
  , testCaseDuration :: Double
  , testCaseResult :: Maybe Result
  }
  deriving stock Show

data Location = Location
  { locationFile :: FilePath
  , locationLine :: Natural
  }
  deriving stock Show

data Result = Failure Text Text | Skipped Text
  deriving stock Show
