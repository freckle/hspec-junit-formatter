module Test.Hspec.JUnitFormatter
  ( use
  , register
  , formatter
  , formatterWith
  , module Api
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
import Test.Hspec.Api.Format.V1 as Api
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Config.Env
import Test.Hspec.JUnit.Render (renderJUnit)
import qualified Test.Hspec.JUnit.Schema as Schema
import Text.XML.Stream.Render (def, renderBytes)

-- | Make `formatter` available for use with @--format@ and use it by default.
use :: SpecWith a -> SpecWith a
use = (modifyConfig (useFormatter formatter) >>)

-- | Make `formatter` available for use with @--format@.
register :: SpecWith a -> SpecWith a
register = (modifyConfig (registerFormatter formatter) >>)


formatter :: (String, FormatConfig -> IO Format)
formatter = ("junit", \_config -> format <$> envJUnitConfig)

formatterWith :: JUnitConfig -> (String, FormatConfig -> IO Format)
formatterWith junitConfig = ("junit", \_config -> return $ format junitConfig)

format :: JUnitConfig -> Format
format junitConfig = \case
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
          suite $ uncurry (itemToTestCase applyPrefix group) <$> items
        }

    runConduitRes
      $ sourceList [output]
      .| renderJUnit
      .| renderBytes def
      .| sinkFile file
 where
  file = getJUnitConfigOutputFile junitConfig
  suiteName = getJUnitConfigSuiteName junitConfig
  applyPrefix = getJUnitPrefixSourcePath junitConfig

groupItems :: [(Path, Item)] -> [(Text, [(Text, Item)])]
groupItems = Map.toList . Map.fromListWith (<>) . fmap group
 where
  group ((path, name), item) =
    (T.intercalate "/" $ pack <$> path, [(pack name, item)])

itemToTestCase
  :: (FilePath -> FilePath) -> Text -> Text -> Item -> Schema.TestCase
itemToTestCase applyPrefix group name item = Schema.TestCase
  { testCaseLocation =
    toSchemaLocation applyPrefix
      <$> (itemResultLocation item <|> itemLocation item)
  , testCaseClassName = group
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
toSchemaLocation applyPrefix Location {..} = Schema.Location
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
  where lines' = T.lines . pack $ show found
