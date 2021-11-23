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

defaultJUnitConfig :: Text -> JUnitConfig
defaultJUnitConfig name = JUnitConfig
  { junitConfigOutputDirectory = "."
  , junitConfigOutputName = "junit.xml"
  , junitConfigOutputFile = Nothing
  , junitConfigSuiteName = name
  }

setJUnitConfigOutputDirectory :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputDirectory x config =
  config { junitConfigOutputDirectory = x }

setJUnitConfigOutputName :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputName x config = config { junitConfigOutputName = x }

setJUnitConfigOutputFile :: FilePath -> JUnitConfig -> JUnitConfig
setJUnitConfigOutputFile x config = config { junitConfigOutputFile = Just x }

getJUnitConfigOutputFile :: JUnitConfig -> FilePath
getJUnitConfigOutputFile JUnitConfig {..} = fromMaybe
  (junitConfigOutputDirectory </> junitConfigOutputName)
  junitConfigOutputFile

getJUnitConfigSuiteName :: JUnitConfig -> Text
getJUnitConfigSuiteName = junitConfigSuiteName
