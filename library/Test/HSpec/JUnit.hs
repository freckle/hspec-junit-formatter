module Test.HSpec.JUnit
  ( runJUnitSpec
  , configWith
  ) where

import Prelude

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Combinators (sinkFile)
import qualified Data.Text as T
import System.IO.Temp (emptySystemTempFile)
import System.Directory (createDirectoryIfMissing)
import Test.Hspec (Spec)
import Test.Hspec.Formatters (Formatter(..), writeLine)
import Test.HSpec.JUnit.Parse (parseJUnit, denormalize)
import Test.HSpec.JUnit.Render (renderJUnit)
import Test.Hspec.Runner (Config(..), Summary, runSpec)
import Text.XML.Stream.Parse (parseFile)
import Text.XML.Stream.Render (def, renderBytes)

runJUnitSpec :: Spec -> (FilePath, String) -> Config -> IO Summary
runJUnitSpec spec (path, name) config = do
  tempFile <- emptySystemTempFile $ "hspec-junit-" <> name
  summary <- spec `runSpec` configWith tempFile name config
  createDirectoryIfMissing True dirPath
  runResourceT
    . runConduit
    $ parseFile def tempFile
    .| parseJUnit
    -- HSpec's formatter cannot correctly output JUnit, so we must denormalize
    -- nested <testsuite /> elements.
    .| denormalize
    .| renderJUnit
    .| renderBytes def
    .| sinkFile (dirPath <> "/test_results.xml")
  pure summary
  where dirPath = path <> "/" <> name

configWith :: FilePath -> String -> Config -> Config
configWith filePath name config = config
  { configFormatter = Just $ junitFormatter name
  , configOutputFile = Right filePath
  }

junitFormatter :: String -> Formatter
junitFormatter suiteName = Formatter
  { headerFormatter = do
    writeLine "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    writeLine $ "<testsuites name=" <> show suiteName <> ">"
  -- TODO needs: package, id, timestamp, hostname, tests, failures, errors, time
  , exampleGroupStarted = \_paths name ->
    writeLine $ "<testsuite name=" <> show (fixBrackets name) <> ">"
  , exampleGroupDone = writeLine "</testsuite>"
  , exampleProgress = \_ _ -> pure ()
  , exampleSucceeded = \path _info ->
    writeLine $ "<testcase name=" <> mkName path <> "/>"
  , exampleFailed = \path _info reason -> do
    writeLine $ "<testcase name=" <> mkName path <> ">"
    writeLine $ "<failure type=\"error\">" <> mkReason reason <> "</failure>"
    writeLine "</testcase>"
  , examplePending = \path _info _reason -> do
    writeLine $ "<testcase name=" <> mkName path <> ">"
    --writeLine $ "<failure type=\"pending\">" <> mkReason reason <> "</failure>"
    writeLine "</testcase>"
  , failedFormatter = pure ()
  , footerFormatter = writeLine "</testsuites>"
  }
 where
  mkName = show . fixBrackets . snd
  fixBrackets =
    T.replace "\"" "&quot;"
      . T.replace "<" "&lt;"
      . T.replace ">" "&gt;"
      . T.replace "&" "&amp;"
      . T.pack
  mkReason = T.unpack . fixBrackets . show
