{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Hspec.JUnit.FormatterSpec
  ( spec
  ) where

import Prelude

import Control.Monad (void)
import Data.Fixed (Micro, Nano)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Time.ISO8601 (parseISO8601)
import Example qualified
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Formatter qualified as Formatter
import Test.Hspec.Runner qualified as Hspec
import Text.Read (readMaybe)
import Text.XML qualified as XML
import Text.XML.Cursor

spec :: Spec
spec = do
  context "ExampleSpec" $ do
    doc <- runIO $ fromDocument <$> renderJUnitXml testConfig Example.spec

    context "time attributes" $ do
      it "sums correctly on testsuites and testsuite" $ do
        let
          -- Do the summation at Nano...
          readNano :: Text -> Nano
          readNano = maybe 0 realToFrac . readMaybe @Double . unpack

          -- ...but truncate to Micro for comparison
          readTimes :: [[Text]] -> Micro
          readTimes = realToFrac . sum . concatMap (map readNano)

          suitesTime :: Micro
          suitesTime =
            readTimes
              $ doc
                $| element "testsuites"
                &| attribute "time"

          suiteTimes :: Micro
          suiteTimes =
            readTimes
              $ doc
                $| element "testsuites"
                &/ element "testsuite"
                &| attribute "time"

          casesTimes :: Micro
          casesTimes =
            readTimes
              $ doc
                $| element "testsuites"
                &/ element "testsuite"
                &/ element "testcase"
                &| attribute "time"

        suitesTime `shouldSatisfy` (> 0)
        suitesTime `shouldBe` suiteTimes
        suitesTime `shouldBe` casesTimes

    context "timestamp attributes" $ do
      it "has the same timestamp on testsuites and testsuite" $ do
        let
          [suitesTimestamp] =
            concatMap (map (parseISO8601 . unpack))
              $ doc
                $| element "testsuites"
                &| attribute "timestamp"

          [suite1Timestamp, suite2Timestamp] =
            concatMap (map (parseISO8601 . unpack))
              $ doc
                $| element "testsuites"
                &/ element "testsuite"
                &| attribute "timestamp"

        suitesTimestamp `shouldSatisfy` isJust
        suite1Timestamp `shouldSatisfy` isJust
        suite2Timestamp `shouldSatisfy` isJust
        suite1Timestamp `shouldBe` suitesTimestamp
        suite2Timestamp `shouldBe` suitesTimestamp

    context "testsuites node" $ do
      it "produces one, with our package name" $ do
        ( doc
            $| element "testsuites"
            &| attribute "name"
          )
          `shouldBe` [["hspec-junit-format"]]

    context "testsuites nodes" $ do
      it "produces two" $ do
        ( doc
            $| element "testsuites"
            &/ element "testsuite"
          )
          `shouldSatisfy` (== 2) . length

      it "has name" $ do
        ( doc
            $| element "testsuites"
            &/ element "testsuite"
            &| attribute "name"
          )
          `shouldBe` [ ["Some section"]
                     , ["Some section/A grouped context"]
                     ]

    context "testcase nodes" $ do
      it "has time" $ do
        ( doc
            $| element "testsuites"
            &/ element "testsuite"
            &/ element "testcase"
            &| attribute "time"
          )
          `shouldSatisfy` (== 6) . length

      it "has timestamp" $ do
        ( doc
            $| element "testsuites"
            &/ element "testsuite"
            &/ element "testcase"
            &| attribute "timestamp"
          )
          `shouldSatisfy` (== 6) . length

      it "has classname" $ do
        ( doc
            $| element "testsuites"
            &/ element "testsuite"
            &/ element "testcase"
            &| attribute "classname"
          )
          `shouldBe` ( replicate 2 ["Some section"]
                         <> replicate 4 ["Some section/A grouped context"]
                     )

      it "has file" $ do
        ( doc
            $| element "testsuites"
            &/ element "testsuite"
            &/ element "testcase"
            &| attribute "file"
          )
          `shouldBe` replicate 6 ["tests/Example.hs"]

      it "has names" $ do
        ( doc
            $| element "testsuites"
            &/ element "testsuite"
            &/ element "testcase"
            &| attribute "name"
          )
          `shouldBe` [ ["has an expectation"]
                     , ["has a failure"]
                     , ["happens in a group"]
                     , ["also happens in a group"]
                     , ["gets skipped"]
                     , ["throws a colourful exception"]
                     ]

    context "first testsuite" $ do
      let [testsuite, _] = doc $| element "testsuites" &/ element "testsuite"

      it "has correct counts" $ do
        (testsuite $| attribute "errors") `shouldBe` ["0"]
        (testsuite $| attribute "failures") `shouldBe` ["1"]
        (testsuite $| attribute "skipped") `shouldBe` ["0"]
        (testsuite $| attribute "tests") `shouldBe` ["2"]

      it "produces two testcase nodes" $ do
        (testsuite $/ element "testcase") `shouldSatisfy` (== 2) . length

      context "first testcase" $ do
        let [testcase, _] = testsuite $/ element "testcase"

        it "has line" $ do
          (testcase $| attribute "line") `shouldBe` ["16"]

        it "has name" $ do
          (testcase $| attribute "name") `shouldBe` ["has an expectation"]

        it "has no child nodes" $ do
          (testcase $| child &| node) `shouldBe` []

      context "second testcase" $ do
        let [_, testcase] = testsuite $/ element "testcase"

        it "has line" $ do
          (testcase $| attribute "line") `shouldBe` ["19"]

        it "has name" $ do
          (testcase $| attribute "name") `shouldBe` ["has a failure"]

        it "has a failure reported" $ do
          ( testcase
              $/ element "failure"
              &/ content
            )
            `shouldBe` [ T.unlines
                           [ "Expected: \"False\""
                           , " but got: \"True\""
                           ]
                       ]

    context "second testsuite" $ do
      let [_, testsuite] =
            doc
              $| element "testsuites"
              &/ element "testsuite"

      it "has correct counts" $ do
        (testsuite $| attribute "errors") `shouldBe` ["1"]
        (testsuite $| attribute "failures") `shouldBe` ["0"]
        (testsuite $| attribute "skipped") `shouldBe` ["1"]
        (testsuite $| attribute "tests") `shouldBe` ["4"]

      it "produces four testcase nodes" $ do
        (testsuite $/ element "testcase") `shouldSatisfy` (== 4) . length

      context "first testcase" $ do
        let [testcase, _, _, _] = testsuite $/ element "testcase"

        it "has line" $ do
          (testcase $| attribute "line") `shouldBe` ["22"]

        it "has name" $ do
          (testcase $| attribute "name") `shouldBe` ["happens in a group"]

        it "has no child nodes" $ do
          (testcase $| child &| node) `shouldBe` []

      context "second testcase" $ do
        let [_, testcase, _, _] = testsuite $/ element "testcase"

        it "has line" $ do
          (testcase $| attribute "line") `shouldBe` ["24"]

        it "has name" $ do
          (testcase $| attribute "name") `shouldBe` ["also happens in a group"]

        it "has no child nodes" $ do
          (testcase $| child &| node) `shouldBe` []

      context "third testcase" $ do
        let [_, _, testcase, _] = testsuite $/ element "testcase"

        it "has line" $ do
          (testcase $| attribute "line") `shouldBe` ["27"]

        it "has name" $ do
          (testcase $| attribute "name") `shouldBe` ["gets skipped"]

        it "has a skipped node" $ do
          ( testcase
              $/ element "skipped"
              &| attribute "message"
            )
            `shouldBe` [["some reason"]]

      context "fourth testcase" $ do
        let [_, _, _, testcase] = testsuite $/ element "testcase"

        it "has line" $ do
          pendingWith "Newer base returns line 29"
          (testcase $| attribute "line") `shouldBe` ["28"]

        it "has name" $ do
          (testcase $| attribute "name") `shouldBe` ["throws a colourful exception"]

        it "has a failure reported" $ do
          pendingWith "Newer base results in a path-prefixed message"
          ( testcase
              $/ element "failure"
              &/ content
            )
            `shouldBe` ["ColourfulException"]

  context "ExampleSpec with prefixing" $ do
    let testConfig' = setJUnitConfigSourcePathPrefix "lol/monorepo" testConfig
    doc <- runIO $ fromDocument <$> renderJUnitXml testConfig' Example.spec

    it "prefixes testcase file attributes" $ do
      ( doc
          $| element "testsuites"
          &/ element "testsuite"
          &/ element "testcase"
          &| attribute "file"
        )
        `shouldBe` replicate 6 ["lol/monorepo/tests/Example.hs"]

testConfig :: JUnitConfig
testConfig = defaultJUnitConfig "hspec-junit-format"

renderJUnitXml :: JUnitConfig -> Spec -> IO XML.Document
renderJUnitXml baseConfig x = do
  withSystemTempDirectory "" $ \tmp -> do
    let junitConfig =
          setJUnitConfigOutputDirectory tmp
            $ setJUnitConfigOutputName "test.xml" baseConfig

    (config, forest) <-
      Hspec.evalSpec Hspec.defaultConfig $ Formatter.use junitConfig x

    void $ Hspec.runSpecForest forest config

    XML.readFile XML.def $ tmp </> "test.xml"
