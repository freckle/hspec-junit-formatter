-- | Load a 'JUnitConfig' using environment variables
module Test.Hspec.JUnit.Config.Env
  ( envJUnitEnabled
  , envJUnitConfig
  ) where

import Prelude

import Data.Semigroup (Endo (..))
import Data.Text (pack)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeBaseName)
import Test.Hspec.JUnit.Config

-- | Is @JUNIT_ENABLED=1@ set in the environment?
envJUnitEnabled :: IO Bool
envJUnitEnabled = (== Just "1") <$> lookupEnv (envPrefix <> "ENABLED")

-- | Produce a 'JUnitConfig' by reading environment variables
--
-- Variable names align with setter functions from "Test.Hspec.JUnit.Config":
--
-- * @JUNIT_OUTPUT_DIRECTORY@ 'setJUnitConfigOutputDirectory'
-- * @JUNIT_OUTPUT_NAME@ 'setJUnitConfigOutputName
-- * and so on
envJUnitConfig :: IO JUnitConfig
envJUnitConfig = do
  modify <-
    appEndo . foldMap Endo
      <$> sequence
        [ lookupEnvOverride "OUTPUT_DIRECTORY" setJUnitConfigOutputDirectory
        , lookupEnvOverride "OUTPUT_NAME" setJUnitConfigOutputName
        , lookupEnvOverride "OUTPUT_FILE" setJUnitConfigOutputFile
        , lookupEnvOverride "SUITE_NAME" $ setJUnitConfigSuiteName . pack
        , lookupEnvOverride "SOURCE_PATH_PREFIX" setJUnitConfigSourcePathPrefix
        , lookupEnvOverride "DROP_CONSOLE_FORMATTING" $
            setJUnitConfigDropConsoleFormatting . (== "1")
        ]

  modify . defaultJUnitConfig . pack . takeBaseName <$> getCurrentDirectory

lookupEnvOverride
  :: String
  -> (String -> JUnitConfig -> JUnitConfig)
  -> IO (JUnitConfig -> JUnitConfig)
lookupEnvOverride name setter =
  maybe id setter <$> lookupEnv (envPrefix <> name)

envPrefix :: String
envPrefix = "JUNIT_"
