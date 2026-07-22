module Test.Hspec.JUnit.Render
  ( renderTestsuites
  ) where

import Prelude

import Conduit (ConduitT)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Array qualified as Array
import Data.Fixed (Nano)
import Data.Foldable (traverse_)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo (..))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Time.ISO8601 (formatISO8601)
import Data.XML.Types (Event)
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Schema
import Text.Regex.Base qualified as Regex
import Text.Regex.TDFA.Text (Regex)
import Text.XML.Stream.Render (attr, content, optionalAttr, tag)

renderTestsuites
  :: MonadReader JUnitConfig m
  => Testsuites
  -> ConduitT i Event m ()
renderTestsuites s = do
  tag
    "testsuites"
    ( attr "name" (unAttr id s.name)
        <> attr "tests" (unAttr (pack . show) s.tests)
        <> attr "failures" (unAttr (pack . show) s.failures)
        <> attr "errors" (unAttr (pack . show) s.errors)
        <> attr "skipped" (unAttr (pack . show) s.skipped)
        <> attr "assertions" (unAttr (pack . show) s.assertions)
        <> attr "time" (unAttr unSeconds s.time)
        <> attr "timestamp" (unAttr (pack . formatISO8601) s.timestamp)
    )
    $ traverse_ renderTestsuite s.children

renderTestsuite
  :: MonadReader JUnitConfig m
  => Testsuite
  -> ConduitT i Event m ()
renderTestsuite s = do
  prefix <- asks getJUnitPrefixSourcePath

  tag
    "testsuite"
    ( attr "name" (unAttr id s.name)
        <> attr "tests" (unAttr (pack . show) s.tests)
        <> attr "failures" (unAttr (pack . show) s.failures)
        <> attr "errors" (unAttr (pack . show) s.errors)
        <> attr "skipped" (unAttr (pack . show) s.skipped)
        <> attr "assertions" (unAttr (pack . show) s.assertions)
        <> attr "time" (unAttr unSeconds s.time)
        <> attr "timestamp" (unAttr (pack . formatISO8601) s.timestamp)
        <> optionalAttr "file" (unAttr (pack . prefix . dropPrefix "./") <$> s.file)
    )
    $ traverse_ renderTestsuiteChild s.children

renderTestsuiteChild
  :: MonadReader JUnitConfig m
  => TestsuiteChild
  -> ConduitT i Event m ()
renderTestsuiteChild = \case
  TestsuiteProperties x -> renderProperties x
  TestsuiteSystemOut x -> renderSystemOut x
  TestsuiteSystemErr x -> renderSystemErr x
  TestsuiteTestcase x -> renderTestcase x
  TestsuiteTestsuite x -> renderTestsuite x

renderProperties
  :: MonadReader JUnitConfig m
  => Properties
  -> ConduitT i Event m ()
renderProperties ps = unless (null ps.unwrap) $ do
  tag "properties" mempty $ traverse_ renderProperty $ ps.unwrap

renderProperty
  :: MonadReader JUnitConfig m
  => Property
  -> ConduitT i Event m ()
renderProperty p = case p.value of
  Left a ->
    tag "property" (attr "name" (unAttr id p.name) <> attr "value" (unAttr id a))
      $ pure ()
  Right c -> tag "property" (attr "name" (unAttr id p.name)) $ renderContent c

renderSystemOut
  :: MonadReader JUnitConfig m
  => SystemOut
  -> ConduitT i Event m ()
renderSystemOut = tag "system-out" mempty . renderContent . (.unwrap)

renderSystemErr
  :: MonadReader JUnitConfig m
  => SystemErr
  -> ConduitT i Event m ()
renderSystemErr = tag "system-err" mempty . renderContent . (.unwrap)

renderTestcase
  :: MonadReader JUnitConfig m
  => Testcase
  -> ConduitT i Event m ()
renderTestcase c = do
  prefix <- asks getJUnitPrefixSourcePath

  tag
    "testcase"
    ( attr "name" (unAttr id c.name)
        <> attr "classname" (unAttr id c.classname)
        <> attr "assertions" (unAttr (pack . show) c.assertions)
        <> attr "time" (unAttr unSeconds c.time)
        <> optionalAttr "file" (unAttr (pack . prefix . dropPrefix "./") <$> c.file)
        <> optionalAttr "line" (unAttr (pack . show) <$> c.line)
    )
    $ do
      traverse_ renderProperties c.properties
      traverse_ (either renderSystemOut renderSystemErr) c.systemStream
      renderTestcaseChild c.child

renderTestcaseChild
  :: MonadReader JUnitConfig m
  => TestcaseChild
  -> ConduitT i Event m ()
renderTestcaseChild = \case
  TestcaseEmpty -> pure ()
  TestcaseSkipped x -> renderSkipped x
  TestcaseFailure x -> renderFailure x
  TestcaseError x -> renderError x

renderSkipped
  :: MonadReader JUnitConfig m
  => Skipped
  -> ConduitT i Event m ()
renderSkipped s = tag "skipped" (attr "message" (unAttr id s.message)) $ pure ()

renderFailure
  :: MonadReader JUnitConfig m
  => Failure
  -> ConduitT i Event m ()
renderFailure f =
  tag
    "failure"
    (attr "message" (unAttr id f.message) <> attr "type" (unAttr id f.type_))
    $ renderContent f.content

renderError
  :: MonadReader JUnitConfig m
  => Error
  -> ConduitT i Event m ()
renderError e =
  tag
    "error"
    (attr "message" (unAttr id e.message) <> attr "type" (unAttr id e.type_))
    $ renderContent e.content

renderContent
  :: MonadReader JUnitConfig m
  => Content
  -> ConduitT i Event m ()
renderContent c = do
  shouldDropConsoleFormatting <- asks getJUnitConfigDropConsoleFormatting

  let clean =
        if shouldDropConsoleFormatting
          then dropConsoleFormatting
          else id

  content $ clean $ c.unwrap

unSeconds :: Seconds -> Text
unSeconds (Seconds d) = pack $ show $ realToFrac @_ @Nano d

-- | Safely render an attribute
--
-- HTML encoding is handled by @xml-conduit@, but additionally we:
--
-- * Drop console formatting (always)
-- * Extra-escape unsupported characters (e.g @\a@ becomes @\\a@)
unAttr :: (a -> Text) -> Attr a -> Text
unAttr f = escapeIllegal . dropConsoleFormatting . f . (.unwrap)

escapeIllegal :: Text -> Text
escapeIllegal =
  appEndo
    $ foldMap
      (\(a, b) -> Endo $ T.replace a b)
      [ ("\0", "\\0")
      , ("\a", "\\a")
      , ("\b", "\\b")
      , ("\f", "\\f")
      , ("\v", "\\v")
      , ("\ESC", "\\ESC")
      ]

-- | Drop ANSI control characters which might be used to set colors
--
-- We always do this in attribute values, since they'd only break XML, but we
-- only do it in content if configured by options, since we want any content
-- node changes to be opt-in.
dropConsoleFormatting :: Text -> Text
dropConsoleFormatting input =
  foldr (dropBetween . (Array.! 0)) input $ Regex.matchAll escRegex input

escRegex :: Regex
escRegex = Regex.makeRegex ("\x1b\\[[0-9;]*[mGKHF]" :: Text)

dropBetween :: (Int, Int) -> Text -> Text
dropBetween (offset, len) input = begining <> end
 where
  (begining, rest) = T.splitAt offset input
  (_, end) = T.splitAt len rest

-- |
--
-- Inlined from <https://hackage-content.haskell.org/package/extra-1.8.1/docs/src/Data.List.Extra.html#dropPrefix>
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix a b = fromMaybe b $ stripPrefix a b
