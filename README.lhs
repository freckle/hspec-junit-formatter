# hspec-junit-formatter

[![Hackage](https://img.shields.io/hackage/v/hspec-junit-formatter.svg?style=flat)](https://hackage.haskell.org/package/hspec-junit-formatter)
[![Stackage Nightly](http://stackage.org/package/hspec-junit-formatter/badge/nightly)](http://stackage.org/nightly/package/hspec-junit-formatter)
[![Stackage LTS](http://stackage.org/package/hspec-junit-formatter/badge/lts)](http://stackage.org/lts/package/hspec-junit-formatter)
[![CI](https://github.com/freckle/hspec-junit-formatter/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/hspec-junit-formatter/actions/workflows/ci.yml)

A `JUnit` XML runner/formatter for [`hspec`](http://hspec.github.io/).

<!--
```haskell
module Main (main) where
import Prelude
import Text.Markdown.Unlit ()
```
-->

## Usage

```haskell
import Test.Hspec
import Test.Hspec.JUnit
import System.Environment (setEnv)

main :: IO ()
main = do
  -- Most likely done in your CI setup
  setEnv "JUNIT_ENABLED" "1"
  setEnv "JUNIT_OUTPUT_DIRECTORY" "/tmp"
  setEnv "JUNIT_SUITE_NAME" "my-tests"

  hspecJUnit spec

spec :: Spec
spec = describe "Addition" $ do
  it "adds" $ do
    2 + 2 `shouldBe` (4 :: Int)
```

---

[LICENSE](./LICENSE)
