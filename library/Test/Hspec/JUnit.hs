module Test.Hspec.JUnit
    ( configWithJUnit
    , junitFormat
    ) where

import Prelude

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
import Test.Hspec.Core.Runner
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Render (renderJUnit)
import qualified Test.Hspec.JUnit.Schema as Schema
import Text.XML.Stream.Render (def, renderBytes)

configWithJUnit :: JUnitConfig -> Config -> Config
configWithJUnit junitConfig config =
  config { configFormat = Just $ junitFormat junitConfig }

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
      output = Schema.Suites
        { suitesName = suiteName
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
 where
  file = getJUnitConfigOutputFile junitConfig
  suiteName = getJUnitConfigSuiteName junitConfig

groupItems :: [(Path, Item)] -> [(Text, [(Text, Item)])]
groupItems = Map.toList . Map.fromListWith (<>) . fmap group
 where
  group ((path, name), item) =
    (T.intercalate "/" $ pack <$> path, [(pack name, item)])

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
        pack
        mMessage
    Failure mLocation reason ->
      Just
        $ Schema.Failure "error"
        $ prefixLocation mLocation
        $ prefixInfo
        $ case reason of
            Error _ err -> pack $ show err
            NoReason -> "no reason"
            Reason err -> pack err
            ExpectedButGot preface expected actual ->
              prefixInfo
                $ T.unlines
                $ pack
                <$> fromMaybe "" preface
                : (foundLines "expected" expected
                  <> foundLines " but got" actual
                  )
  }
 where
  prefixLocation mLocation str = case mLocation of
    Nothing -> str
    Just Location {..} ->
      mconcat
          [ pack locationFile
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

unSeconds :: Seconds -> Double
unSeconds (Seconds x) = x

foundLines :: Show a => Text -> a -> [String]
foundLines msg found = case lines' of
  [] -> []
  first : rest ->
    unpack (msg <> ": " <> first) : (unpack . (T.replicate 9 " " <>) <$> rest)
  where lines' = T.lines . pack $ show found
