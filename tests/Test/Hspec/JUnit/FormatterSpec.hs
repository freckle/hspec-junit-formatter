{-# LANGUAGE CPP #-}

module Test.Hspec.JUnit.FormatterSpec
  ( spec
  ) where

import Prelude

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as LT
import Example qualified
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Golden
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Formatter qualified as Formatter
import Test.Hspec.Runner
import Text.Pretty.Simple (pShowNoColor)
import Text.XML qualified as XML
import Text.XML.Stream.Render.Internal qualified as XML

spec :: Spec
spec = do
  it "matches golden file" $ do
    junitGolden "default" id

  it "matches golden file with prefixing" $ do
    junitGolden "prefixed" $ setJUnitConfigSourcePathPrefix "lol/monorepo"

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
          modifyConfig
            $ setJUnitConfigPretty True
            $ setJUnitConfigOutputDirectory tmp
            $ setJUnitConfigOutputName "test.xml"
            $ defaultJUnitConfig "hspec-junit-format"

    runSpec' $ Formatter.use junitConfig Example.spec
    readNormalizedXML $ tmp </> "test.xml"

  pure
    Golden
      { output = actual
      , encodePretty = LT.unpack . pShowNoColor
      , writeToFile = XML.writeFile (XML.def {XML.rsPretty = True})
      , readFromFile = readNormalizedXML
      , goldenFile = "tests" </> "golden" </> name <> suffix <.> "xml"
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
normalizeDoc = removeTimeAttributes

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

-- Store a separate golden for newer base because throwIO's behavior changed
-- such that different location data is present and so our output changes
suffix :: String
#if MIN_VERSION_base(4,21,0)
suffix = "-base-4.21"
#else
suffix = ""
#endif
