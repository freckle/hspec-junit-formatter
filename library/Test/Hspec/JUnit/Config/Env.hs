-- | Load a 'JUnitConfig' using environment variables
module Test.Hspec.JUnit.Config.Env
  ( envJUnitEnabled
  , envJUnitConfig

    -- * Exported for testing
  , readJUnitConfig
  ) where

import Prelude

import Data.Semigroup (Endo (..))
import Data.Text (pack)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv)
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
-- * @JUNIT_OUTPUT_NAME@ 'setJUnitConfigOutputName'
-- * and so on
envJUnitConfig :: IO JUnitConfig
envJUnitConfig = do
  env <- getEnvironment
  base <- takeBaseName <$> getCurrentDirectory
  pure $ readJUnitConfig base env

readJUnitConfig :: FilePath -> [(String, String)] -> JUnitConfig
readJUnitConfig base env = modify $ defaultJUnitConfig $ pack base
 where
  modify =
    appEndo $
      foldMap
        Endo
        [ readEnv "OUTPUT_DIRECTORY" setJUnitConfigOutputDirectory
        , readEnv "OUTPUT_NAME" setJUnitConfigOutputName
        , readEnv "OUTPUT_FILE" setJUnitConfigOutputFile
        , readEnv "SUITE_NAME" $ setJUnitConfigSuiteName . pack
        , readEnv "SOURCE_PATH_PREFIX" setJUnitConfigSourcePathPrefix
        , readEnv "DROP_CONSOLE_FORMATTING" $
            setJUnitConfigDropConsoleFormatting . (== "1")
        ]

  readEnv name setter =
    maybe id setter $ lookup (envPrefix <> name) env

envPrefix :: String
envPrefix = "JUNIT_"
