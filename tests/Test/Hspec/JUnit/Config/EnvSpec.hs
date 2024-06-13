module Test.Hspec.JUnit.Config.EnvSpec
  ( spec
  ) where

import Prelude

import Test.Hspec
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Config.Env

spec :: Spec
spec = do
  describe "envJUnitConfig" $ do
    it "has sensible defaults" $ do
      let config = readJUnitConfig "base-directory" []

      getJUnitConfigOutputFile config `shouldBe` "./junit.xml"
      getJUnitConfigSuiteName config `shouldBe` "base-directory"
      getJUnitPrefixSourcePath config "path" `shouldBe` "path"
      getJUnitConfigDropConsoleFormatting config `shouldBe` False

    it "respects JUNIT_OUTPUT_DIRECTORY" $ do
      let config =
            readJUnitConfig
              "base-directory"
              [("JUNIT_OUTPUT_DIRECTORY", "/tmp")]

      getJUnitConfigOutputFile config `shouldBe` "/tmp/junit.xml"

    it "respects JUNIT_OUTPUT_NAME" $ do
      let config =
            readJUnitConfig
              "base-directory"
              [("JUNIT_OUTPUT_NAME", "report.xml")]

      getJUnitConfigOutputFile config `shouldBe` "./report.xml"

    it "respects JUNIT_OUTPUT_FILE" $ do
      let config =
            readJUnitConfig
              "base-directory"
              [ ("JUNIT_OUTPUT_DIRECTORY", "/home")
              , ("JUNIT_OUTPUT_NAME", "results.xml")
              , ("JUNIT_OUTPUT_FILE", "/tmp/report.xml")
              ]

      getJUnitConfigOutputFile config `shouldBe` "/tmp/report.xml"

    it "respects JUNIT_SUITE_NAME" $ do
      let config =
            readJUnitConfig
              "base-directory"
              [("JUNIT_SUITE_NAME", "hspec")]

      getJUnitConfigSuiteName config `shouldBe` "hspec"

    it "respects JUNIT_SOURCE_PATH_PREFIX" $ do
      let config =
            readJUnitConfig
              "base-directory"
              [("JUNIT_SOURCE_PATH_PREFIX", "package")]

      getJUnitPrefixSourcePath config "path" `shouldBe` "package/path"

    it "handles trailing-slash in JUNIT_SOURCE_PATH_PREFIX" $ do
      let config =
            readJUnitConfig
              "base-directory"
              [("JUNIT_SOURCE_PATH_PREFIX", "package/")]

      getJUnitPrefixSourcePath config "path" `shouldBe` "package/path"

    it "respects JUNIT_DROP_CONSOLE_FORMATTING" $ do
      let
        config1 =
          readJUnitConfig
            "base-directory"
            [("JUNIT_DROP_CONSOLE_FORMATTING", "1")]
        config0 =
          readJUnitConfig
            "base-directory"
            [("JUNIT_DROP_CONSOLE_FORMATTING", "0")]

      getJUnitConfigDropConsoleFormatting config1 `shouldBe` True
      getJUnitConfigDropConsoleFormatting config0 `shouldBe` False
