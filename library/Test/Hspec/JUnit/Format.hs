module Test.Hspec.JUnit.Format
  ( junit
  ) where

import Prelude

import Conduit (runConduitRes, sinkFile, yield, (.|))
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.Hspec.Api.Format.V1
import Test.Hspec.JUnit.Config as Config
import Test.Hspec.JUnit.Render (renderJUnit)
import Test.Hspec.JUnit.Schema qualified as Schema
import Text.XML.Stream.Render (def, renderBytes)
import Text.XML.Stream.Render.Internal (rsPretty)

junit :: JUnitConfig -> FormatConfig -> IO Format
junit junitConfig _config = pure $ \case
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
          { name = suiteName
          , suites =
              groups <&> \(group, items) -> do
                let suite xs =
                      Schema.Suite
                        { name = group
                        , timestamp = time
                        , cases = xs
                        }
                suite $ uncurry (itemToTestCase applyPrefix group) <$> items
          }

    runConduitRes
      $ yield output
        .| renderJUnit dropConsoleFormatting
        .| renderBytes renderConfig
        .| sinkFile file
 where
  file = getJUnitConfigOutputFile junitConfig
  suiteName = getJUnitConfigSuiteName junitConfig
  applyPrefix = getJUnitPrefixSourcePath junitConfig
  dropConsoleFormatting = getJUnitConfigDropConsoleFormatting junitConfig
  renderConfig =
    if getJUnitConfigPretty junitConfig
      then def {rsPretty = True}
      else def

groupItems :: [(Path, Item)] -> [(Text, [(Text, Item)])]
groupItems = Map.toList . Map.fromListWith (<>) . fmap group
 where
  group ((path, name), item) =
    (T.intercalate "/" $ pack <$> path, [(pack name, item)])

itemToTestCase
  :: (FilePath -> FilePath) -> Text -> Text -> Item -> Schema.TestCase
itemToTestCase applyPrefix group name item =
  Schema.TestCase
    { location =
        toSchemaLocation applyPrefix
          <$> (itemResultLocation item <|> itemLocation item)
    , className = group
    , name = name
    , duration = unSeconds $ itemDuration item
    , result = case itemResult item of
        Success -> Nothing
        Pending mLocation mMessage ->
          Just
            $ Schema.Skipped
            $ prefixLocation mLocation
            $ prefixInfo
            $ maybe "" pack mMessage
        Failure mLocation reason ->
          Just
            $ Schema.Failure "error"
            $ prefixLocation mLocation
            $ prefixInfo
            $ reasonToText reason
    }
 where
  prefixLocation mLocation str = case mLocation of
    Nothing -> str
    Just l ->
      mconcat
        [ pack $ applyPrefix $ locationFile l
        , ":"
        , pack $ show $ locationLine l
        , ":"
        , pack $ show $ locationColumn l
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
toSchemaLocation applyPrefix l =
  Schema.Location
    { Schema.file = applyPrefix $ locationFile l
    , Schema.line = fromIntegral $ max 0 $ locationLine l
    }

unSeconds :: Seconds -> Double
unSeconds (Seconds x) = x

reasonToText :: FailureReason -> Text
reasonToText = \case
  Error _ err -> pack $ show err
  NoReason -> "no reason"
  Reason err -> pack err
  ExpectedButGot preface expected actual ->
    T.unlines
      $ pack
        <$> fromMaybe "" preface
          : ( foundLines "expected" expected
                <> foundLines " but got" actual
            )

foundLines :: Show a => Text -> a -> [String]
foundLines msg found = case lines' of
  [] -> []
  first : rest ->
    unpack (msg <> ": " <> first) : (unpack . (T.replicate 9 " " <>) <$> rest)
 where
  lines' = T.lines . pack $ show found
