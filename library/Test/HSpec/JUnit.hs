module Test.HSpec.JUnit
  ( junitFormat
  , runJUnitSpec
  , configWith
  ) where

import Prelude

import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Combinators (sinkFile)
import Data.Conduit.List (sourceList)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.HSpec.JUnit.Render (renderJUnit)
import qualified Test.HSpec.JUnit.Schema as Schema
import Test.Hspec.Core.Format
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec (Spec)
import Text.XML.Stream.Render (def, renderBytes)

runJUnitSpec :: Spec -> (FilePath, String) -> Config -> IO Summary
runJUnitSpec spec (path, name) config =
  spec `runSpec` configWith filePath name config
  where filePath = path <> "/" <> name <> "/test_results.xml"

configWith :: FilePath -> String -> Config -> Config
configWith filePath name config =
  config { configFormat = Just $ junitFormat filePath name }

-- | Output `hspec` results as a `JUnit` `XML` file.
junitFormat
  :: FilePath -- ^ File path for resulting xml file. E.G. `my-dir/output.xml`
  -> String -- ^ Name of the test suite
  -> FormatConfig
  -> IO Format
junitFormat file suiteName _config = pure $ \case
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

    let groups = groupItems paths
    let
      output = Schema.Suites
        { suitesName = T.pack suiteName
        , suitesSuites = groups <&> \(group, items) -> do
          let
            suite xs = Schema.Suite
              { suiteName = group
              , suiteTimestamp = time
              , suiteCases = xs
              }
          suite $ uncurry (itemToTestCase group) <$> items
        }
    runConduitRes
      $ sourceList [output]
      .| renderJUnit
      .| renderBytes def
      .| sinkFile file

groupItems :: [(Path, Item)] -> [(Text, [(Text, Item)])]
groupItems = Map.toList . Map.fromListWith (<>) . fmap group
 where
  group ((path, name), item) =
    (T.intercalate "/" $ T.pack <$> path, [(T.pack name, item)])

itemToTestCase :: Text -> Text -> Item -> Schema.TestCase
itemToTestCase group name item = Schema.TestCase
  { testCaseClassName = group
  , testCaseName = name
  , testCaseDuration = unSeconds $ itemDuration item
  , testCaseResult = case itemResult item of
    Success -> Nothing
    Pending mLocation mMessage ->
      Just $ Schema.Skipped $ prefixLocation mLocation $ prefixInfo $ maybe
        ""
        T.pack
        mMessage
    Failure mLocation reason ->
      Just
        $ Schema.Failure "error"
        $ prefixLocation mLocation
        $ prefixInfo
        $ case reason of
            Error _ err -> T.pack $ show err
            NoReason -> "no reason"
            Reason err -> T.pack err
            ExpectedButGot preface expected actual ->
              prefixInfo
                $ T.unlines
                $ T.pack
                <$> fromMaybe "" preface
                : (foundLines "expected" expected
                  <> foundLines " but got" actual
                  )
  }
 where
  prefixLocation mLocation str = case mLocation of
    Nothing -> str
    Just Location {..} ->
      T.concat
          [ T.pack locationFile
          , ":"
          , T.pack $ show locationLine
          , ":"
          , T.pack $ show locationColumn
          , "\n"
          ]
        <> str
  prefixInfo str
    | T.null $ T.strip $ T.pack $ itemInfo item = str
    | otherwise = T.pack (itemInfo item) <> "\n\n" <> str

unSeconds :: Seconds -> Double
unSeconds (Seconds x) = x

foundLines :: Show a => Text -> a -> [String]
foundLines msg found = case lines' of
  [] -> []
  first : rest ->
    T.unpack (msg <> ": " <> first)
      : (T.unpack . (T.replicate 9 " " <>) <$> rest)
  where lines' = T.lines . T.pack $ show found
