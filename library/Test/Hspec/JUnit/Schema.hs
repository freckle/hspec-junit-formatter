module Test.Hspec.JUnit.Schema
  ( Suites (..)
  , Suite (..)
  , TestCase (..)
  , Location (..)
  , Result (..)
  ) where

import Prelude

import Data.Text (Text)
import Data.Time (UTCTime)
import Numeric.Natural

data Suites = Suites
  { name :: Text
  , suites :: [Suite]
  }
  deriving stock (Show)

data Suite = Suite
  { name :: Text
  , timestamp :: UTCTime
  , cases :: [TestCase]
  }
  deriving stock (Show)

data TestCase = TestCase
  { location :: Maybe Location
  , className :: Text
  , name :: Text
  , duration :: Double
  , result :: Maybe Result
  }
  deriving stock (Show)

data Location = Location
  { file :: FilePath
  , line :: Natural
  }
  deriving stock (Show)

data Result = Failure Text Text | Skipped Text
  deriving stock (Show)
