{-# LANGUAGE CPP #-}

-- | Compatibility module for 'configAvailableFormatters'
--
-- This feature is only available in hspec >= 2.9. Note that this module doesn't
-- do anything to backport the feature; it just supplies a function that will
-- use it when possible and safely no-op when not.
module Test.Hspec.Core.Runner.Ext
  ( configAddAvailableFormatter
  ) where

import Prelude

import Test.Hspec.Core.Format (Format, FormatConfig)
import Test.Hspec.Core.Runner (Config (..))

configAddAvailableFormatter
  :: String -> (FormatConfig -> IO Format) -> Config -> Config
#if MIN_VERSION_hspec_core(2,9,0)
configAddAvailableFormatter name format config = config
  { configAvailableFormatters =
    configAvailableFormatters config <> [(name, format)]
  }
#else
configAddAvailableFormatter _ _ = id
#endif
