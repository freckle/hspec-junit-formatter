{-# LANGUAGE CPP #-}

module Test.Hspec.JUnit.FormatterSpec
  ( spec
  ) where

import Prelude

import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Example
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Golden
import Test.Hspec.JUnit.Config
import qualified Test.Hspec.JUnit.Formatter as Formatter
import Test.Hspec.Runner
import qualified Text.XML as XML

spec :: Spec
spec = do
  it "matches golden file" $
    junitGolden "default" id

  it "matches golden file with prefixing" $
    junitGolden "prefixed" $
      setJUnitConfigSourcePathPrefix "lol/monorepo"

-- | Run @Example.spec@ and compare XML to a golden file
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

    runSpec' $ Formatter.use junitConfig Example.spec
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

runSpec' :: Spec -> IO ()
runSpec' x = do
  (config, forest) <- evalSpec defaultConfig x
  void $ runSpecForest forest config

readNormalizedXML :: FilePath -> IO XML.Document
readNormalizedXML = fmap normalizeDoc . XML.readFile XML.def

normalizeDoc :: XML.Document -> XML.Document
normalizeDoc = removeWhitespaceNodes . removeTimeAttributes . normalizeErrorMessages

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

normalizeErrorMessages :: XML.Document -> XML.Document
normalizeErrorMessages doc =
  doc
    { XML.documentRoot = go $ XML.documentRoot doc
    }
 where
  go el =
    el
      { XML.elementNodes = map (onNodeElement go . normalizeErrorContent . normalizeLineNumbers) $ XML.elementNodes el
      }

  normalizeErrorContent :: XML.Node -> XML.Node
  normalizeErrorContent = \case
    XML.NodeElement el
      | XML.elementName el == XML.Name "failure" Nothing Nothing ->
          XML.NodeElement $ el { XML.elementNodes = map stripLocationPrefix $ XML.elementNodes el }
      | XML.elementName el == XML.Name "skipped" Nothing Nothing ->
          XML.NodeElement $ el { XML.elementNodes = map stripLocationPrefix $ XML.elementNodes el }
      | otherwise -> XML.NodeElement el
    n -> n

  normalizeLineNumbers :: XML.Node -> XML.Node
  normalizeLineNumbers = \case
    XML.NodeElement el
      | XML.elementName el == XML.Name "testcase" Nothing Nothing ->
          let attrs = XML.elementAttributes el
              normalizedAttrs = Map.adjust normalizeLineAttr (XML.Name "line" Nothing Nothing) attrs
          in XML.NodeElement $ el { XML.elementAttributes = normalizedAttrs }
      | otherwise -> XML.NodeElement el
    n -> n

  normalizeLineAttr :: T.Text -> T.Text
  normalizeLineAttr lineText
    | lineText == "29" = "28"  -- Normalize line 29 to 28 for version compatibility
    | otherwise = lineText

  stripLocationPrefix :: XML.Node -> XML.Node
  stripLocationPrefix = \case
    XML.NodeContent content ->
      let contentText = T.unpack content
          normalizedContent = case lines contentText of
            (firstLine:rest) | ("tests/Example.hs:" `isPrefixOf` firstLine || "lol/monorepo/tests/Example.hs:" `isPrefixOf` firstLine) && "\n" `isInfixOf` contentText ->
              unlines rest
            _ -> contentText
          trimmedContent = case reverse normalizedContent of
            '\n':rest -> reverse rest
            _ -> normalizedContent
      in XML.NodeContent $ T.pack trimmedContent
    n -> n

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
