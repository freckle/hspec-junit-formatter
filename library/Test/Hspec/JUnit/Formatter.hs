-- | An Hspec formatter that produces a JUnit XML file of results
--
-- @
-- -- file test/SpecHook.hs
-- module SpecHook where
--
-- import Test.Hspec
-- import Test.Hspec.JUnit.Config
-- import Test.Hspec.JUnit.Formatter qualified as Formatter
--
-- -- | To only produce a JUnit file, silencing other output
-- hook :: Spec -> Spec
-- hook = Formatter.use defaultJUnitConfig
--
-- -- | To produce a JUnit file /in addition/ to the normal output
-- hook :: Spec -> Spec
-- hook = Formatter.add defaultJUnitConfig
--
-- -- | To only produce, but only when @--format=junit@ is used
-- hook :: Spec -> Spec
-- hook = Formatter.register defaultJUnitConfig
-- @
--
-- See also,
--
-- - https://hspec.github.io/hspec-discover.html#spec-hooks
-- - https://hspec.github.io/extending-hspec-formatter.html#packaging-a-formatter-for-distribution-and-reuse
--
-- For a version that reads configuration from @JUNIT_@ environment variables,
-- see "Test.Hspec.JUnit.Formatter.Env".
module Test.Hspec.JUnit.Formatter
  ( use
  , add
  , register
  , formatter
  , module Test.Hspec.JUnit.Config
  , module Api
  ) where

import Prelude

import Data.Maybe (fromMaybe)

import Test.Hspec.Api.Format.V1 as Api
import qualified Test.Hspec.Core.Format as Core
import qualified Test.Hspec.Core.Formatters.V2 as V2
import Test.Hspec.Core.Runner as Core (Config (..))
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Format

-- | Register 'junit' as an available formatter and use it by default
use :: JUnitConfig -> SpecWith a -> SpecWith a
use config = (modifyConfig (useFormatter $ formatter config) >>)

-- | Register 'junit', and use it /in addition/ to the default
add :: JUnitConfig -> SpecWith a -> SpecWith a
add config = (modifyConfig (addFormatter $ formatter config) >>)

-- | Register 'junit', but do not change the default
register :: JUnitConfig -> SpecWith a -> SpecWith a
register config = (modifyConfig (registerFormatter $ formatter config) >>)

formatter :: JUnitConfig -> (String, FormatConfig -> IO Format)
formatter config = ("junit", junit config)

addFormatter :: (String, FormatConfig -> IO Format) -> Config -> Config
addFormatter f = go . registerFormatter f
 where
  go config =
    config
      { configFormat =
          Just
            . (`addFormat` format)
            . fromMaybe defaultFormat
            $ configFormat config
      }

  format = snd $ liftFormatter f

defaultFormat :: Core.FormatConfig -> IO Core.Format
defaultFormat = V2.formatterToFormat V2.checks

addFormat
  :: (Core.FormatConfig -> IO Core.Format)
  -> (Core.FormatConfig -> IO Core.Format)
  -> Core.FormatConfig
  -> IO Core.Format
addFormat f1 f2 fc = do
  formatEvent1 <- f1 fc
  formatEvent2 <- f2 fc
  pure $ \event -> do
    formatEvent1 event
    formatEvent2 event
