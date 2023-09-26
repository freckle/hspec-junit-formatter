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
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Golden
import Test.Hspec.JUnit
import Test.Hspec.Runner
import qualified Text.XML as XML

main :: IO ()
main = hspec $ do
  describe "XML output" $ do
    it "matches golden file" $
      junitGolden "default" id

    it "matches golden file with prefixing" $
      junitGolden "prefixed" $
        setJUnitConfigSourcePathPrefix "lol/monorepo"

-- | Run @ExampleSpec.spec@ and compare XML to a golden file
junitGolden
  :: String
  -- ^ Unique name
  -> (JUnitConfig -> JUnitConfig)
  -- ^ Any modification to make to the 'JUnitConfig' before running
  -> IO (Golden XML.Document)
junitGolden name modifyConfig = do
  actual <- withSystemTempDirectory "" $ \tmp -> do
    let junitConfig =
          modifyConfig $
            setJUnitConfigOutputDirectory tmp $
              setJUnitConfigOutputName "test.xml" $
                defaultJUnitConfig "hspec-junit-format"

    void $ runSpec ExampleSpec.spec $ configWithJUnit junitConfig defaultConfig
    readNormalizedXML $ tmp </> "test.xml"

  pure
    Golden
      { output = actual
      , encodePretty = show
      , writeToFile = XML.writeFile XML.def
      , readFromFile = readNormalizedXML
      , goldenFile =
          "tests" </> "golden" </> name <> "-" <> ghcSuffix <.> "xml"
      , actualFile = Nothing
      , failFirstTime = False
      }

readNormalizedXML :: FilePath -> IO XML.Document
readNormalizedXML = fmap normalizeDoc . XML.readFile XML.def

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

-- GHC can change certain aspects, mainly about source-location, so we can
-- incorpate that by tracking separate Golden files as necessary
ghcSuffix :: String
#if __GLASGOW_HASKELL__ >= 900
ghcSuffix = "ghc-9"
#elif __GLASGOW_HASKELL__ >= 800
ghcSuffix = "ghc-8"
#else
-- Fail to compile on other GHCs
#endif
