# hspec-junit-formatter

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
import Test.Hspec.Core.Runner (defaultConfig, hspecWith)
import Test.Hspec.JUnit
import Test.Hspec.JUnit.Config

main :: IO ()
main = do
  let
    junitConfig = setJUnitConfigOutputDirectory "/tmp" $ defaultJUnitConfig "my-tests"
    hspecConfig = configWithJUnit junitConfig defaultConfig

  hspecWith hspecConfig spec

spec :: Spec
spec = describe "Addition" $ do
  it "adds" $ do
    2 + 2 `shouldBe` (4 :: Int)
```

---

[LICENSE](./LICENSE)
