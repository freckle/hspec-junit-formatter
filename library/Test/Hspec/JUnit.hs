{-# LANGUAGE CPP #-}

module Test.Hspec.JUnit
  ( -- * Runners
    hspecJUnit
  , hspecJUnitWith

    -- * Directly modifying 'Config'
  , configWithJUnit
  , configWithJUnitAvailable

    -- * Actual format function
  , junitFormat

    -- * Configuration
  , module Test.Hspec.JUnit.Config
  ) where

import Prelude

import Control.Applicative ((<|>))
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Combinators (sinkFile)
import Data.Conduit.List (sourceList)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.Hspec.Core.Format
  ( Event (..)
  , FailureReason (..)
  , Format
  , FormatConfig
  , Item (..)
  , Location (..)
  , Path
  , Result (..)
  , Seconds (..)
  )
import Test.Hspec.Core.Runner (Config (..), defaultConfig, hspecWith)
import Test.Hspec.Core.Runner.Ext (configAddAvailableFormatter)
import Test.Hspec.Core.Spec (Spec)
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Config.Env
import Test.Hspec.JUnit.Render (renderJUnit)
import qualified Test.Hspec.JUnit.Schema as Schema
import Text.XML.Stream.Render (def, renderBytes)

-- | Like 'hspec' but adds JUNit functionality
--
-- To actually /use/ the JUnit format, you must set @JUNIT_ENABLED=1@ in the
-- environment; by default, this function just behaves like 'hspec'.
--
-- (If using hspec >= 2.9, running tests with @--test-arguments="-f junit"@ also
-- works.)
--
-- All configuration of the JUnit report occurs through environment variables.
--
-- See "Test.Hspec.JUnit.Config" and "Test.Hspec.JUnit.Config.Env".
hspecJUnit :: Spec -> IO ()
hspecJUnit = hspecJUnitWith defaultConfig

-- | 'hspecJUnit' but built on a non-default 'Config'
hspecJUnitWith :: Config -> Spec -> IO ()
hspecJUnitWith config spec = do
  junitEnabled <- envJUnitEnabled
  junitConfig <- envJUnitConfig

  let
    modify = if junitEnabled then configWithJUnit junitConfig else id
    base = configWithJUnitAvailable junitConfig config

  hspecWith (modify base) spec

-- | Modify an Hspec 'Config' to use 'junitFormat'
configWithJUnit :: JUnitConfig -> Config -> Config
configWithJUnit junitConfig config =
  config {configFormat = Just $ junitFormat junitConfig}

-- | Modify an Hspec 'Config' to have the 'junitFormat' /available/
--
-- Adds @junit@ to the list of available options for @-f, --format@.
--
-- __NOTE__: This only works with hspec >= 2.9, otherwise it is a no-op.
configWithJUnitAvailable :: JUnitConfig -> Config -> Config
configWithJUnitAvailable = configAddAvailableFormatter "junit" . junitFormat

-- | Hspec 'configFormat' that generates a JUnit report
junitFormat :: JUnitConfig -> FormatConfig -> IO Format
junitFormat junitConfig _config = pure $ \case
  Started -> pure ()
  GroupStarted _ -> pure ()
  GroupDone _ -> pure ()
  Progress _ _ -> pure ()
  ItemStarted _ -> pure ()
  ItemDone _ _ -> pure ()
  Done paths -> do
    time <- getCurrentTime

    let (directory, _) = splitFileName file
    createDirectoryIfMissing True directory

    let
      groups = groupItems paths
      output =
        Schema.Suites
          { suitesName = suiteName
          , suitesSuites =
              groups <&> \(group, items) -> do
                let suite xs =
                      Schema.Suite
                        { suiteName = group
                        , suiteTimestamp = time
                        , suiteCases = xs
                        }
                suite $ uncurry (itemToTestCase applyPrefix group) <$> items
          }

    runConduitRes $
      sourceList [output]
        .| renderJUnit dropConsoleFormatting
        .| renderBytes def
        .| sinkFile file
 where
  file = getJUnitConfigOutputFile junitConfig
  suiteName = getJUnitConfigSuiteName junitConfig
  applyPrefix = getJUnitPrefixSourcePath junitConfig
  dropConsoleFormatting = getJUnitConfigDropConsoleFormatting junitConfig

groupItems :: [(Path, Item)] -> [(Text, [(Text, Item)])]
groupItems = Map.toList . Map.fromListWith (<>) . fmap group
 where
  group ((path, name), item) =
    (T.intercalate "/" $ pack <$> path, [(pack name, item)])

itemToTestCase
  :: (FilePath -> FilePath) -> Text -> Text -> Item -> Schema.TestCase
itemToTestCase applyPrefix group name item =
  Schema.TestCase
    { testCaseLocation =
        toSchemaLocation applyPrefix
          <$> (itemResultLocation item <|> itemLocation item)
    , testCaseClassName = group
    , testCaseName = name
    , testCaseDuration = unSeconds $ itemDuration item
    , testCaseResult = case itemResult item of
        Success -> Nothing
        Pending mLocation mMessage ->
          Just $
            Schema.Skipped $
              prefixLocation mLocation $
                prefixInfo $
                  maybe
                    ""
                    pack
                    mMessage
        Failure mLocation reason ->
          Just $
            Schema.Failure "error" $
              prefixLocation mLocation $
                prefixInfo $
                  reasonToText reason
    }
 where
  prefixLocation mLocation str = case mLocation of
    Nothing -> str
    Just Location {..} ->
      mconcat
        [ pack $ applyPrefix locationFile
        , ":"
        , pack $ show locationLine
        , ":"
        , pack $ show locationColumn
        , "\n"
        ]
        <> str
  prefixInfo str
    | T.null $ T.strip $ pack $ itemInfo item = str
    | otherwise = pack (itemInfo item) <> "\n\n" <> str

itemResultLocation :: Item -> Maybe Location
itemResultLocation item = case itemResult item of
  Success -> Nothing
  Pending mLocation _ -> mLocation
  Failure mLocation _ -> mLocation

toSchemaLocation :: (FilePath -> FilePath) -> Location -> Schema.Location
toSchemaLocation applyPrefix Location {..} =
  Schema.Location
    { Schema.locationFile = applyPrefix locationFile
    , Schema.locationLine = fromIntegral $ max 0 locationLine
    }

unSeconds :: Seconds -> Double
unSeconds (Seconds x) = x

foundLines :: Show a => Text -> a -> [String]
foundLines msg found = case lines' of
  [] -> []
  first : rest ->
    unpack (msg <> ": " <> first) : (unpack . (T.replicate 9 " " <>) <$> rest)
 where
  lines' = T.lines . pack $ show found

{- FOURMOLU_DISABLE -}
reasonToText :: FailureReason -> Text
reasonToText = \case
  Error _ err -> pack $ show err
  NoReason -> "no reason"
  Reason err -> pack err
#if MIN_VERSION_hspec_core(2,11,0)
  ColorizedReason err -> pack err
#endif
  ExpectedButGot preface expected actual ->
    T.unlines
      $ pack
      <$> fromMaybe "" preface
      : (foundLines "expected" expected
        <> foundLines " but got" actual
        )
{- FOURMOLU_ENABLE -}
