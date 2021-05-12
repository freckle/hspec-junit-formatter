module Test.HSpec.JUnit.Schema
  ( Suites(..)
  , Suite(..)
  , TestCase(..)
  , Result(..)
  ) where

import Prelude

import Data.Text (Text)

data Suites = Suites
  { suitesName :: Text
  , suitesSuites :: [Suite]
  }
  deriving Show

data Suite = Suite
  { suiteName :: Text
  , suiteCases :: [TestCase]
  }
  deriving Show

data TestCase = TestCase
  { testCaseClassName :: Text
  , testCaseName :: Text
  , testCaseDuration :: Double
  , testCaseResult :: Maybe Result
  }
  deriving Show

data Result = Failure Text Text | Skipped Text
  deriving (Show)
