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
