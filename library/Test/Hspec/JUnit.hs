module Test.Hspec.JUnit
  (
  -- * Runners
    hspecJUnit
  , hspecJUnitWith

  -- * Directly modifying 'Config'
  , configWithJUnit
  , configWithJUnitAvailable

  -- * Actual format function
  , junitFormat

  -- * Configuration
  , module Test.Hspec.JUnit.Config
  ) where

import Prelude

import Control.Applicative ((<|>))
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Combinators (sinkFile)
import Data.Conduit.List (sourceList)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.Hspec.Core.Format
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Runner.Ext
import Test.Hspec.Core.Spec (Spec)
import Test.Hspec.Api.Format.V1 (liftFormatter)
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Config.Env
import Test.Hspec.JUnit.Render (renderJUnit)
import qualified Test.Hspec.JUnit.Schema as Schema
import Test.Hspec.JUnitFormatter (formatterWith)
import Text.XML.Stream.Render (def, renderBytes)

-- | Like 'hspec' but adds JUNit functionality
--
-- To actually /use/ the JUnit format, you must set @JUNIT_ENABLED=1@ in the
-- environment; by default, this function just behaves like 'hspec'.
--
-- (If using hspec >= 2.9, running tests with @--test-arguments="-f junit"@ also
-- works.)
--
-- All configuration of the JUnit report occurs through environment variables.
--
-- See "Test.Hspec.JUnit.Config" and "Test.Hspec.JUnit.Config.Env".
--
hspecJUnit :: Spec -> IO ()
hspecJUnit = hspecJUnitWith defaultConfig

-- | 'hspecJUnit' but built on a non-default 'Config'
hspecJUnitWith :: Config -> Spec -> IO ()
hspecJUnitWith config spec = do
  junitEnabled <- envJUnitEnabled
  junitConfig <- envJUnitConfig

  let
    modify = if junitEnabled then configWithJUnit junitConfig else id
    base = configWithJUnitAvailable junitConfig config

  hspecWith (modify base) spec

-- | Modify an Hspec 'Config' to use 'junitFormat'
configWithJUnit :: JUnitConfig -> Config -> Config
configWithJUnit junitConfig config =
  config { configFormat = Just $ junitFormat junitConfig }

-- | Modify an Hspec 'Config' to have the 'junitFormat' /available/
--
-- Adds @junit@ to the list of available options for @-f, --format@.
--
-- __NOTE__: This only works with hspec >= 2.9, otherwise it is a no-op.
--
configWithJUnitAvailable :: JUnitConfig -> Config -> Config
configWithJUnitAvailable = configAddAvailableFormatter "junit" . junitFormat

-- | Hspec 'configFormat' that generates a JUnit report
junitFormat :: JUnitConfig -> FormatConfig -> IO Format
junitFormat = snd . liftFormatter . formatterWith
