module Main
  ( main
  )
where

import Prelude

import Control.Monad (void)
import Test.HSpec.JUnit (junitFormat)
import Test.Hspec
import Test.Hspec.Runner

main :: IO ()
main = do
  summary <- flip runSpec defaultConfig $ do
    describe "XML output" $ do
      it "matches golen file" $ do
        -- pendingWith
        --   "Need to normalize time and timestamp values to match golden"
        makeJunitResults

        golden <- readFile "tests/golden.xml"
        test <- readFile ".temp/test.xml"

        test `shouldBe` golden

  evaluateSummary summary

makeJunitResults :: IO ()
makeJunitResults = void $ flip runSpec config $ do
  describe "Some section" $ do
    it "has an expectation" $ do
      True `shouldBe` True
    it "has a failure" $ do
      True `shouldBe` False

    context "A grouped context" $ do
      it "happens in a group" $ do
        True `shouldBe` True
      it "also happens in a group" $ do
        True `shouldBe` True
      it "gets skipped" $ do
        pendingWith "some reason"

config :: Config
config = defaultConfig
  { configFormat = Just $ junitFormat ".temp/test.xml" "hspec-junit-format"
  }
