module Test.HSpec.JUnit
  {-# DEPRECATED "Use Test.Hspec.JUnit instead" #-}
  ( junitFormat
  , runJUnitSpec
  , configWith
  ) where

import Prelude

import Data.Text (pack)
import Test.Hspec.Core.Format
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec (Spec)
import Test.Hspec.JUnit hiding (junitFormat)
import qualified Test.Hspec.JUnit as JUnit

runJUnitSpec :: Spec -> (FilePath, String) -> Config -> IO Summary
runJUnitSpec spec (path, name) config =
  spec `runSpec` configWith filePath name config
  where filePath = path <> "/" <> name <> "/test_results.xml"

configWith :: FilePath -> String -> Config -> Config
configWith filePath =
  configWithJUnit
    . setJUnitConfigOutputFile filePath
    . defaultJUnitConfig
    . pack

junitFormat :: FilePath -> String -> FormatConfig -> IO Format
junitFormat filePath =
  JUnit.junitFormat
    . setJUnitConfigOutputFile filePath
    . defaultJUnitConfig
    . pack
