-- | Example used by golden testing of XML reports
--
-- The path name and line numbers of this file are part of the golden report
-- being tested with. So if this file changes, they will need to be regenerated.
-- In fact, this sentence was added to retain them across a formatting change.
module ExampleSpec (spec) where

import Control.Exception
import Prelude

import Test.Hspec

spec :: Spec
spec = do
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
      it "throws a colourful exception" $ do
        throwIO ColourfulException :: IO ()

data ColourfulException = ColourfulException
  deriving stock (Show)

instance Exception ColourfulException where
  displayException _ = "\x1b[32mColour\x1b[31mful\x1b[0mException"
