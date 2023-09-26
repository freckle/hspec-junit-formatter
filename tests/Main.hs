{-# LANGUAGE CPP #-}

module Main
  ( main
  )
where

import Prelude

import Control.Monad (void)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified ExampleSpec
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.JUnit
import Test.Hspec.Runner
import qualified Text.XML as XML

main :: IO ()
main = hspec $ do
  describe "XML output" $ do
    it "matches golden file" $ do
      withJUnitReport ExampleSpec.spec $ \doc -> do
        golden <- XML.readFile XML.def goldenPath
        normalizeDoc doc `shouldBe` normalizeDoc golden

    it "matches golden file with prefixing" $ do
      let modify = setJUnitConfigSourcePathPrefix "lol/monorepo"

      withJUnitReportConfig modify ExampleSpec.spec $ \doc -> do
        golden <- XML.readFile XML.def goldenPrefixedPath
        normalizeDoc doc `shouldBe` normalizeDoc golden

withJUnitReport :: Spec -> (XML.Document -> IO ()) -> IO ()
withJUnitReport = withJUnitReportConfig id

withJUnitReportConfig
  :: (JUnitConfig -> JUnitConfig) -> Spec -> (XML.Document -> IO ()) -> IO ()
withJUnitReportConfig modifyConfig spec f =
  withSystemTempDirectory "" $ \tmp -> do
    let
      path = tmp </> "test.xml"
      junitConfig =
        modifyConfig $
          setJUnitConfigOutputDirectory tmp $
            setJUnitConfigOutputName "test.xml" $
              defaultJUnitConfig "hspec-junit-format"
      hspecConfig = configWithJUnit junitConfig defaultConfig
    void $ runSpec spec hspecConfig
    f =<< XML.readFile XML.def path

normalizeDoc :: XML.Document -> XML.Document
normalizeDoc = removeWhitespaceNodes . removeTimeAttributes

removeWhitespaceNodes :: XML.Document -> XML.Document
removeWhitespaceNodes doc =
  doc
    { XML.documentRoot = go $ XML.documentRoot doc
    }
 where
  go el =
    el {XML.elementNodes = concatMap filterWhitespace $ XML.elementNodes el}

  filterWhitespace :: XML.Node -> [XML.Node]
  filterWhitespace = \case
    XML.NodeElement el -> [XML.NodeElement $ go el]
    XML.NodeContent c | T.all isSpace c -> []
    n -> [n]

-- | Remove volatile attributes so they don't invalidate comparison
removeTimeAttributes :: XML.Document -> XML.Document
removeTimeAttributes =
  removeAttributesByName "time" . removeAttributesByName "timestamp"

removeAttributesByName :: XML.Name -> XML.Document -> XML.Document
removeAttributesByName name doc =
  doc
    { XML.documentRoot = go $ XML.documentRoot doc
    }
 where
  go el =
    el
      { XML.elementAttributes = Map.delete name $ XML.elementAttributes el
      , XML.elementNodes = map (onNodeElement go) $ XML.elementNodes el
      }

  onNodeElement f = \case
    XML.NodeElement el -> XML.NodeElement $ f el
    n -> n

-- GHC 9 changes the bounds of the SrcLoc from HasCallStack, which changes the
-- line numbers reported by Hspec. Fun.
--
-- True `shouldBe` False
-- \^ -- Before GHC 9 reports from here (18:7 in golden.xml)
--
-- True `shouldBe` False
--      ^-- GHC 9 reports from here (18:12 in golden-ghc-9.xml)
--
-- We should really re-consider the golden testing approach. If we were instead
-- asserting on parsed XML, we'd have a lot more options here.

goldenPath, goldenPrefixedPath :: FilePath
#if __GLASGOW_HASKELL__ >= 900
goldenPath = "tests/golden-ghc-9.xml"
goldenPrefixedPath = "tests/golden-prefixed-ghc-9.xml"
#else
goldenPath = "tests/golden.xml"
goldenPrefixedPath = "tests/golden-prefixed.xml"
#endif
