{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Hspec.JUnit.Format
  ( junit
  ) where

import Prelude

import Conduit
import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Control.Monad.Reader (runReaderT)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Monoid (First (..))
import Data.Semigroup (Sum (..))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Text (Text, pack)
import Data.Time (UTCTime, getCurrentTime)
import Data.Typeable (typeOf)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Hspec.Api.Format.V1
  ( Event (..)
  , Format
  , FormatConfig
  , Item (..)
  , Location (..)
  , Path
  )
import Test.Hspec.Api.Format.V1 qualified as Hspec
import Test.Hspec.JUnit.Config as Config
import Test.Hspec.JUnit.Render
import Test.Hspec.JUnit.Schema
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
  Done items -> do
    now <- getCurrentTime
    createDirectoryIfMissing True $ takeDirectory file
    runConduitRes
      $ transPipe (`runReaderT` junitConfig)
      $ renderTestsuites (formatTestsuites now suiteName items)
        .| renderBytes renderConfig
        .| sinkFile file
 where
  file = getJUnitConfigOutputFile junitConfig
  suiteName = getJUnitConfigSuiteName junitConfig
  renderConfig = def {rsPretty = getJUnitConfigPretty junitConfig}

formatTestsuites :: UTCTime -> Text -> [(Path, Item)] -> Testsuites
formatTestsuites now suiteName items =
  Testsuites
    { name = Attr suiteName
    , tests = getSum totals.tests
    , failures = getSum totals.failures
    , errors = getSum totals.errors
    , skipped = getSum totals.skipped
    , assertions = getSum totals.assertions
    , time = getSum totals.time
    , timestamp = Attr now
    , children = suites
    }
 where
  -- Group all items by prefix and then convert each group to a suite. In an
  -- ideal world, we would build a tree instead so we could correctly render
  -- nested contexts as nested testsuites, but that is hard and I'm tired.
  suites :: [Testsuite]
  suites = map (formatTestsuite now) $ NE.groupAllWith (fst . fst) items

  totals :: SuiteTotals
  totals = foldMap getSuiteTotals suites

formatTestsuite :: UTCTime -> NonEmpty (Path, Item) -> Testsuite
formatTestsuite now items =
  Testsuite
    { name = Attr $ pack $ intercalate "/" path
    , tests = getSum totals.tests
    , failures = getSum totals.failures
    , errors = getSum totals.errors
    , skipped = getSum totals.skipped
    , assertions = getSum totals.assertions
    , time = getSum totals.time
    , timestamp = Attr now
    , file = getFirst totals.file
    , children
    }
 where
  path :: [String]
  path = fst $ fst $ NE.head items

  children :: [TestsuiteChild]
  children = NE.toList $ TestsuiteTestcase . formatTestcase <$> items

  totals :: SuiteTotals
  totals = foldMap getSuiteChildTotals children

formatTestcase :: (Path, Item) -> Testcase
formatTestcase ((path, name), item) =
  Testcase
    { name = Attr $ pack name
    , classname = Attr $ pack $ intercalate "/" path
    , assertions = Attr 0 -- not available with Hspec
    , time = Attr $ itemDuration item
    , file = Attr . locationFile <$> location
    , line = Attr . fromIntegral . locationLine <$> location
    , properties = Nothing
    , systemStream = []
    , child
    }
 where
  (location, child) = case itemResult item of
    Hspec.Success ->
      ( itemLocation item
      , TestcaseEmpty
      )
    Hspec.Pending mLocation mMessage ->
      ( mLocation <|> itemLocation item
      , TestcaseSkipped $ Skipped $ maybe "" (Attr . pack) mMessage
      )
    Hspec.Failure mLocation reason ->
      ( mLocation <|> itemLocation item
      , formatFailureReason reason
      )

formatFailureReason :: Hspec.FailureReason -> TestcaseChild
formatFailureReason = \case
  Hspec.NoReason ->
    TestcaseFailure
      $ Failure
        { message = "No reason given"
        , type_ = "Failure.NoReason"
        , content = "No reason given"
        }
  Hspec.Reason msg ->
    TestcaseFailure
      $ Failure
        { message = Attr $ firstLineOf msg
        , type_ = "Failure.Reason"
        , content = Content $ pack $ msg <> "\n"
        }
  Hspec.ExpectedButGot mPrefix expected got ->
    TestcaseFailure
      $ Failure
        { message = Attr "Received value did not match expected"
        , type_ = Attr "Failure.ExpectedButGot"
        , content =
            Content
              $ pack
              $ mconcat
                [ maybe "" (<> "\n") mPrefix
                , "Expected: " <> show expected <> "\n"
                , " but got: " <> show got <> "\n"
                ]
        }
  Hspec.Error mPrefix ex ->
    TestcaseError
      $ Error
        { message = Attr $ firstLineOf $ displayException ex
        , type_ = Attr $ pack $ show $ typeOf ex
        , content =
            Content
              $ pack
              $ mconcat
                [ maybe "" (<> "\n") mPrefix
                , displayException ex <> "\n"
                ]
        }
 where
  firstLineOf =
    maybe "" (pack . NE.head)
      . NE.nonEmpty
      . lines

data SuiteTotals = SuiteTotals
  { tests :: Sum (Attr Natural)
  , failures :: Sum (Attr Natural)
  , errors :: Sum (Attr Natural)
  , skipped :: Sum (Attr Natural)
  , assertions :: Sum (Attr Natural)
  , time :: Sum (Attr Seconds)
  , file :: First (Attr FilePath)
  }
  deriving stock (Generic)
  deriving (Monoid, Semigroup) via GenericSemigroupMonoid SuiteTotals

getSuiteTotals :: Testsuite -> SuiteTotals
getSuiteTotals s = foldMap getSuiteChildTotals s.children

getSuiteChildTotals :: TestsuiteChild -> SuiteTotals
getSuiteChildTotals = \case
  TestsuiteTestcase tc ->
    let
      base :: SuiteTotals
      base = mempty {tests = 1, time = Sum tc.time, file = First tc.file}
    in
      case tc.child of
        TestcaseEmpty -> base
        TestcaseSkipped {} -> base {skipped = 1}
        TestcaseFailure {} -> base {failures = 1}
        TestcaseError {} -> base {errors = 1}
  TestsuiteTestsuite s -> getSuiteTotals s
  _ -> mempty
