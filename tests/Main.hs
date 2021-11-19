module Main
  ( main
  )
where

import Prelude

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified ExampleSpec
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.HSpec.JUnit
import Test.Hspec
import Test.Hspec.Runner
import qualified Text.XML as XML

main :: IO ()
main = hspec $ do
  describe "XML output" $ do
    it "matches golden file" $ do
      withJUNitReport ExampleSpec.spec $ \doc -> do
        golden <- XML.readFile XML.def "tests/golden.xml"
        removeTimeAttributes doc `shouldBe` removeTimeAttributes golden

withJUNitReport :: Spec -> (XML.Document -> IO ()) -> IO ()
withJUNitReport spec f = withSystemTempDirectory "" $ \tmp -> do
  let path = tmp </> "test.xml"
  let config = configWith path "hspec-junit-format" defaultConfig
  void $ runSpec spec config
  f =<< XML.readFile XML.def path

-- | Remove volatile attributes so they don't invalidate comparison
removeTimeAttributes :: XML.Document -> XML.Document
removeTimeAttributes =
  removeAttributesByName "time" . removeAttributesByName "timestamp"

removeAttributesByName :: XML.Name -> XML.Document -> XML.Document
removeAttributesByName name doc = doc
  { XML.documentRoot = go $ XML.documentRoot doc
  }
 where
  go el = el
    { XML.elementAttributes = Map.delete name $ XML.elementAttributes el
    , XML.elementNodes = map (onNodeElement go) $ XML.elementNodes el
    }

  onNodeElement f = \case
    XML.NodeElement el -> XML.NodeElement $ f el
    n -> n
