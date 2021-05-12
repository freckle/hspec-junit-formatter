module Test.HSpec.JUnit
  ( junitFormat
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
import Test.HSpec.JUnit.Render (renderJUnit)
import qualified Test.HSpec.JUnit.Schema as Schema
import Test.Hspec.Core.Format
import Text.XML.Stream.Render (def, renderBytes)

junitFormat :: FilePath -> String -> FormatConfig -> IO Format
junitFormat file suiteName _config = pure $ \case
  Started -> pure ()
  GroupStarted _ -> pure ()
  GroupDone _ -> pure ()
  Progress _ _ -> pure ()
  ItemStarted _ -> pure ()
  ItemDone _ _ -> pure ()
  Done paths -> do
    let suites = Schema.Suites (T.pack suiteName)
    let groups = groupItems paths
    let
      output = suites $ groups <&> \(group, items) -> do
        let suite xs = Schema.Suite { suiteName = group, suiteCases = xs }
        suite $ uncurry (itemToTestCase group) <$> items
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
    Pending _mLocation mMessage ->
      Just $ Schema.Skipped $ maybe "" T.pack mMessage
    Failure _mLocation reason -> Just $ Schema.Failure "error" $ case reason of
      Error _ err -> T.pack $ show err
      NoReason -> "no reason"
      Reason err -> T.pack err
      ExpectedButGot preface expected actual ->
        T.unlines
          $ T.pack
          <$> fromMaybe "" preface
          : (foundLines "expected" expected <> foundLines " but got" actual)
  }

unSeconds :: Seconds -> Double
unSeconds (Seconds x) = x

foundLines :: Show a => Text -> a -> [String]
foundLines msg found = case lines' of
  [] -> []
  first : rest ->
    T.unpack (msg <> ": " <> first)
      : (T.unpack . (T.replicate 9 " " <>) <$> rest)
  where lines' = T.lines . T.pack $ show found
