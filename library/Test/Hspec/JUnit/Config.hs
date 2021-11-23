module Test.Hspec.JUnit.Config
    ( JUnitConfig

    -- * Construction
    , defaultJUnitConfig
    , setJUnitConfigOutputDirectory
    , setJUnitConfigOutputName
    , setJUnitConfigOutputFile

    -- * Use
    , getJUnitConfigOutputFile
    , getJUnitConfigSuiteName
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

-- | Retrieve the full path to the generated report
getJUnitConfigOutputFile :: JUnitConfig -> FilePath
getJUnitConfigOutputFile JUnitConfig {..} = fromMaybe
  (junitConfigOutputDirectory </> junitConfigOutputName)
  junitConfigOutputFile

-- | Retrieve the suite name given on construction
getJUnitConfigSuiteName :: JUnitConfig -> Text
getJUnitConfigSuiteName = junitConfigSuiteName
