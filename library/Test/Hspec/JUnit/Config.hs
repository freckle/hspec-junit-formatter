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
  { outputDirectory :: FilePath
  , outputName :: FilePath
  , outputFile :: Maybe FilePath
  , suiteName :: Text
  , sourcePathPrefix :: Maybe FilePath
  , dropConsoleFormatting :: Bool
  }

-- | Construct a 'JUnitConfig' given a suite name
--
-- See individual set functions for defaults.
defaultJUnitConfig :: Text -> JUnitConfig
defaultJUnitConfig name =
  JUnitConfig
    { outputDirectory = "."
    , outputName = "junit.xml"
    , outputFile = Nothing
    , suiteName = name
    , sourcePathPrefix = Nothing
    , dropConsoleFormatting = False
    }

-- | Set the directory within which to generate the report
--
-- Default is current working directory.
setJUnitConfigOutputDirectory :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputDirectory x config = config {outputDirectory = x}

-- | Set the name for the generated report
--
-- Default is @junit.xml@.
setJUnitConfigOutputName :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputName x config = config {outputName = x}

-- | Set the full path to the generated report
--
-- If given, the directory and name configurations are ignored.
setJUnitConfigOutputFile :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputFile x config = config {outputFile = Just x}

setJUnitConfigSuiteName :: Text -> JUnitConfig -> JUnitConfig
setJUnitConfigSuiteName x config = config {suiteName = x}

-- | Set a prefix to apply to source paths in the report
--
-- Default is none. This can be required if you run specs from a sub-directory
-- in a monorepository, and you need reported paths to be from the repository
-- root.
setJUnitConfigSourcePathPrefix :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigSourcePathPrefix x config = config {sourcePathPrefix = Just x}

-- | Set whether console formatting characters should be dropped from failure
-- reports.
--
-- Default is False. Most XML processors will fail to parse the XML if it
-- contains the ANSI control characters used by console formatting.
setJUnitConfigDropConsoleFormatting :: Bool -> JUnitConfig -> JUnitConfig
setJUnitConfigDropConsoleFormatting x config = config {dropConsoleFormatting = x}

-- | Retrieve the full path to the generated report
getJUnitConfigOutputFile :: JUnitConfig -> FilePath
getJUnitConfigOutputFile c = fromMaybe (c.outputDirectory </> c.outputName) c.outputFile

-- | Retrieve the suite name given on construction
getJUnitConfigSuiteName :: JUnitConfig -> Text
getJUnitConfigSuiteName c = c.suiteName

-- | Retrieve the function to apply to reported source paths
--
-- Will be 'id' if no prefix configured.
getJUnitPrefixSourcePath :: JUnitConfig -> FilePath -> FilePath
getJUnitPrefixSourcePath c = maybe id (</>) c.sourcePathPrefix

-- | Retrieve whether console formatting characters should be dropped from
-- failure reports.
getJUnitConfigDropConsoleFormatting :: JUnitConfig -> Bool
getJUnitConfigDropConsoleFormatting c = c.dropConsoleFormatting
