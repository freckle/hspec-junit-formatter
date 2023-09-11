module Test.Hspec.JUnit.Config
  ( JUnitConfig

  -- * Construction
  , defaultJUnitConfig
  , setJUnitConfigOutputDirectory
  , setJUnitConfigOutputName
  , setJUnitConfigOutputFile
  , setJUnitConfigSuiteName
  , setJUnitConfigSourcePathPrefix
  , setJUnitConfigDropConsoleFormatting

  -- * Use
  , getJUnitConfigOutputFile
  , getJUnitConfigSuiteName
  , getJUnitPrefixSourcePath
  , getJUnitConfigDropConsoleFormatting
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.FilePath ((</>))

data JUnitConfig = JUnitConfig
  { junitConfigOutputDirectory :: FilePath
  , junitConfigOutputName :: FilePath
  , junitConfigOutputFile :: Maybe FilePath
  , junitConfigSuiteName :: Text
  , junitConfigSourcePathPrefix :: Maybe FilePath
  , junitConfigDropConsoleFormatting :: Bool
  }

-- | Construct a 'JUnitConfig' given a suite name
--
-- See individual set functions for defaults.
--
defaultJUnitConfig :: Text -> JUnitConfig
defaultJUnitConfig name = JUnitConfig
  { junitConfigOutputDirectory = "."
  , junitConfigOutputName = "junit.xml"
  , junitConfigOutputFile = Nothing
  , junitConfigSuiteName = name
  , junitConfigSourcePathPrefix = Nothing
  , junitConfigDropConsoleFormatting = False
  }

-- | Set the directory within which to generate the report
--
-- Default is current working directory.
--
setJUnitConfigOutputDirectory :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputDirectory x config =
  config { junitConfigOutputDirectory = x }

-- | Set the name for the generated report
--
-- Default is @junit.xml@.
--
setJUnitConfigOutputName :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputName x config = config { junitConfigOutputName = x }

-- | Set the full path to the generated report
--
-- If given, the directory and name configurations are ignored.
--
setJUnitConfigOutputFile :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputFile x config = config { junitConfigOutputFile = Just x }

setJUnitConfigSuiteName :: Text -> JUnitConfig -> JUnitConfig
setJUnitConfigSuiteName x config = config { junitConfigSuiteName = x }

-- | Set a prefix to apply to source paths in the report
--
-- Default is none. This can be required if you run specs from a sub-directory
-- in a monorepository, and you need reported paths to be from the repository
-- root.
--
setJUnitConfigSourcePathPrefix :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigSourcePathPrefix x config =
  config { junitConfigSourcePathPrefix = Just x }

-- | Set whether console formatting characters should be dropped from failure
-- reports.
--
-- Default is False. Most XML processors will fail to parse the XML if it
-- contains the ANSI control characters used by console formatting.
setJUnitConfigDropConsoleFormatting :: Bool -> JUnitConfig -> JUnitConfig
setJUnitConfigDropConsoleFormatting x config = config { junitConfigDropConsoleFormatting = x }

-- | Retrieve the full path to the generated report
getJUnitConfigOutputFile :: JUnitConfig -> FilePath
getJUnitConfigOutputFile JUnitConfig {..} = fromMaybe
  (junitConfigOutputDirectory </> junitConfigOutputName)
  junitConfigOutputFile

-- | Retrieve the suite name given on construction
getJUnitConfigSuiteName :: JUnitConfig -> Text
getJUnitConfigSuiteName = junitConfigSuiteName

-- | Retrieve the function to apply to reported source paths
--
-- Will be 'id' if no prefix configured.
--
getJUnitPrefixSourcePath :: JUnitConfig -> FilePath -> FilePath
getJUnitPrefixSourcePath JUnitConfig {..} =
  maybe id (</>) junitConfigSourcePathPrefix

-- | Retrieve whether console formatting characters should be dropped from
-- failure reports.
getJUnitConfigDropConsoleFormatting :: JUnitConfig -> Bool
getJUnitConfigDropConsoleFormatting JUnitConfig {..} = junitConfigDropConsoleFormatting
