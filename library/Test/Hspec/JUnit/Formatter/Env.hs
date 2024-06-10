-- | Version of "Test.Hspec.JUnit.Formatter" that reads config from ENV
--
-- @
-- -- file test/SpecHook.hs
-- module SpecHook where
--
-- import Test.Hspec
-- import Test.Hspec.JUnit.Formatter.Env qualified as Formatter
--
-- -- | To always produce a JUnit file, taking all config from the environment
-- hook :: Spec -> Spec
-- hook = Formatter.add
--
-- -- | Same, but only if @JUNIT_ENABLED=1@
-- hook :: Spec -> Spec
-- hook = Formatter.whenEnabled Formatter.add
-- @
module Test.Hspec.JUnit.Formatter.Env
  ( whenEnabled
  , use
  , add
  , register
  , formatter
  , module Api
  ) where

import Prelude

import Test.Hspec.Api.Format.V1 as Api
import Test.Hspec.Core.Spec (runIO)
import Test.Hspec.JUnit.Config
import Test.Hspec.JUnit.Config.Env
import qualified Test.Hspec.JUnit.Formatter as JUnit

whenEnabled :: (SpecWith a -> SpecWith a) -> SpecWith a -> SpecWith a
whenEnabled hook spec = do
  enabled <- runIO envJUnitEnabled
  if enabled then hook spec else spec

withConfig
  :: (JUnitConfig -> SpecWith a -> SpecWith a) -> SpecWith a -> SpecWith a
withConfig toHook spec = do
  config <- runIO envJUnitConfig
  toHook config spec

use :: SpecWith a -> SpecWith a
use = withConfig JUnit.use

add :: SpecWith a -> SpecWith a
add = withConfig JUnit.add

register :: SpecWith a -> SpecWith a
register = withConfig JUnit.register

formatter :: IO (String, FormatConfig -> IO Format)
formatter = JUnit.formatter <$> envJUnitConfig
