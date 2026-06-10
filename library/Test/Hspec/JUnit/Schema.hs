-- | The structure of a JUnit XML file
--
-- This intends to be a complete and accurate representation of
-- <https://github.com/testmoapp/junitxml#complete-junit-xml-example>, though we
-- don't (and sometimes can't) exercise all of it.
module Test.Hspec.JUnit.Schema
  ( Testsuites (..)
  , Testsuite (..)
  , TestsuiteChild (..)
  , Testcase (..)
  , TestcaseChild (..)
  , Skipped (..)
  , Failure (..)
  , Error (..)
  , Properties (..)
  , Property (..)
  , SystemOut (..)
  , SystemErr (..)

    -- * Tagged XML types
  , Attr (..)
  , Content (..)

    -- * Other wrapper types
  , Seconds (..)
  ) where

import Prelude

import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Numeric.Natural (Natural)
import Test.Hspec.Core.Format (Seconds (..))

-- | @<testsuites>@
--
-- Usually the root element of a JUnit XML file. Some tools leave out the
-- @<testsuites>@ element if there is only a single top-level @<testsuite>@
-- element (which is then used as the root element).
data Testsuites = Testsuites
  { name :: Attr Text
  -- ^ Name of the entire test run
  , tests :: Attr Natural
  -- ^ Total number of tests in this file
  , failures :: Attr Natural
  -- ^ Total number of failed tests in this file
  , errors :: Attr Natural
  -- ^ Total number of errored tests in this file
  , skipped :: Attr Natural
  -- ^ Total number of skipped tests in this file
  , assertions :: Attr Natural
  -- ^ Total number of assertions for all tests in this file
  , time :: Attr Seconds
  -- ^ Aggregated time of all tests in this file in seconds
  , timestamp :: Attr UTCTime
  -- ^ Date and time of when the test run was executed (in ISO 8601 format)
  , children :: [Testsuite]
  -- ^ Only @<testsuite>@s can be descendents of this node
  }

-- | @<testsuite>@
--
-- A test suite usually represents a class, folder or group of tests. There can
-- be many test suites in an XML file, and there can be test suites under other
-- test suites.
data Testsuite = Testsuite
  { name :: Attr Text
  -- ^ Name of the test suite (e.g. class name or folder name)
  , tests :: Attr Natural
  -- ^ Total number of tests in this suite
  , failures :: Attr Natural
  -- ^ Total number of failed tests in this suite
  , errors :: Attr Natural
  -- ^ Total number of errored tests in this suite
  , skipped :: Attr Natural
  -- ^ Total number of skipped tests in this suite
  , assertions :: Attr Natural
  -- ^ Total number of assertions for all tests in this suite
  , time :: Attr Seconds
  -- ^ Aggregated time of all tests in this file in seconds
  , timestamp :: Attr UTCTime
  -- ^ Date and time of when the test suite was executed (in ISO 8601 format)
  , file :: Maybe (Attr FilePath)
  -- ^ Source code file of this test suite
  , children :: [TestsuiteChild]
  }

-- | @<testsuite>@ can contain nodes that are any of these
data TestsuiteChild
  = TestsuiteProperties Properties
  | TestsuiteSystemOut SystemOut
  | TestsuiteSystemErr SystemErr
  | TestsuiteTestcase Testcase
  | TestsuiteTestsuite Testsuite

-- | @<properties>@
--
-- Test suites can have additional properties such as environment variables or
-- version numbers. Some tools also support properties for test cases.
newtype Properties = Properties
  { unwrap :: [Property]
  }

-- | @<property>@
--
-- Each property has a name and value. Some tools also support properties with
-- text values instead of value attributes.
data Property = Property
  { name :: Attr Text
  , value :: Either (Attr Text) Content
  }

-- | @<system-out>@
--
-- Optionally data written to standard out for the suite. Also supported on a
-- test case level, see below.
newtype SystemOut = SystemOut
  { unwrap :: Content
  }

-- | @<system-err>@
--
-- Optionally data written to standard error for the suite. Also supported on a
-- test case level, see below.
newtype SystemErr = SystemErr
  { unwrap :: Content
  }

-- | @<testcase>@
--
-- There are one or more test cases in a test suite. A test passed if there
-- isn't an additional result element (skipped, failure, error).
data Testcase = Testcase
  { name :: Attr Text
  -- ^ The name of this test case, often the method name
  , classname :: Attr Text
  -- ^ The name of the parent class/folder, often the same as the suite's name
  , assertions :: Attr Natural
  -- ^ Number of assertions checked during test case execution
  , time :: Attr Seconds
  -- ^ Execution time of the test in seconds
  , file :: Maybe (Attr FilePath)
  -- ^ Source code file of this test case
  , line :: Maybe (Attr Natural)
  -- ^ Source code line number of the start of this test case
  , properties :: Maybe Properties
  , systemStream :: [Either SystemOut SystemErr]
  -- ^ Stored as a combined list to represent ordering and interleaving
  , child :: TestcaseChild
  }

-- | @<testcase>@ will contain only one of these
data TestcaseChild
  = -- | Success case
    TestcaseEmpty
  | TestcaseSkipped Skipped
  | TestcaseFailure Failure
  | TestcaseError Error

-- | @<skipped>@
--
-- Indicates that the test was not executed. Can have an optional message
-- describing why the test was skipped.
newtype Skipped = Skipped
  { message :: Attr Text
  }

-- | @<failure>@
--
-- The test failed because one of the assertions/checks failed. Can have a
-- message and failure type, often the assertion type or class. The text content
-- of the element often includes the failure description or stack trace.
data Failure = Failure
  { message :: Attr Text
  , type_ :: Attr Text
  , content :: Content
  -- ^ Failure description or stack trace
  }

-- | @<error>@
--
-- The test had an unexpected error during execution. Can have a message and
-- error type, often the exception type or class. The text content of the
-- element often includes the error description or stack trace.
data Error = Error
  { message :: Attr Text
  , type_ :: Attr Text
  , content :: Content
  -- ^ Error description or stack trace
  }

newtype Attr a = Attr
  { unwrap :: a
  }
  deriving newtype (IsString, Num)

newtype Content = Content
  { unwrap :: Text
  }
  deriving newtype (IsString)
