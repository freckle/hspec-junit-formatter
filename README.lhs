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
import qualified Test.HSpec.JUnit as JUnit

main :: IO ()
main = do
  let
    report = "/tmp/test-results.xml"
    config = JUnit.configWith report "my-tests" defaultConfig

  hspecWith config spec

spec :: Spec
spec = describe "Addition" $ do
  it "adds" $ do
    2 + 2 `shouldBe` (4 :: Int)
```

---

[LICENSE](./LICENSE)
