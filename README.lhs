# hspec-junit-formatter

[![Hackage](https://img.shields.io/hackage/v/hspec-junit-formatter.svg?style=flat)](https://hackage.haskell.org/package/hspec-junit-formatter)
[![Stackage Nightly](http://stackage.org/package/hspec-junit-formatter/badge/nightly)](http://stackage.org/nightly/package/hspec-junit-formatter)
[![Stackage LTS](http://stackage.org/package/hspec-junit-formatter/badge/lts)](http://stackage.org/lts/package/hspec-junit-formatter)
[![CI](https://github.com/freckle/hspec-junit-formatter/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/hspec-junit-formatter/actions/workflows/ci.yml)

A `JUnit` XML runner/formatter for [`hspec`](http://hspec.github.io/).

<!--
```haskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where
import Prelude
import Text.Markdown.Unlit ()

-- Used in a later example
import qualified Test.Hspec.JUnit.Formatter.Env as FormatterEnv
```
-->

## Usage (with `hspec-discover`)

Place the following in `test/SpecHook.hs`:

```haskell
import Test.Hspec
import Test.Hspec.JUnit.Config
import qualified Test.Hspec.JUnit.Formatter as Formatter

hook :: Spec -> Spec
hook = Formatter.use $ defaultJUnitConfig "test-suite"
```

This _replaces_ the usual formatter, so only a JUnit report is generated and no
other output is visible.

### Registering instead of using

To make the JUnit formatter available for use with `--format`, but not used by
default, use `register`:

```haskell
hook2 :: Spec -> Spec
hook2 = Formatter.register $ defaultJUnitConfig "test-suite"
```


### Adding a JUnit report

To produce a JUnit report _in addition to normal output_, use `add`:

```haskell
hook3 :: Spec -> Spec
hook3 = Formatter.add $ defaultJUnitConfig "test-suite"
```

### Environment Configuration

To configure things via @JUNIT_@-prefixed environment variables, import
`Formatter.Env` instead. It exports all the same functions:

```hs
import qualified Test.Hspec.JUnit.Formatter.Env as FormatterEnv
```

And set the necessary variables,

```
JUNIT_OUTPUT_DIRECTORY=/tmp
JUNIT_SUITE_NAME=my-tests
```

```haskell
hook4 :: Spec -> Spec
hook4 = FormatterEnv.add
```

### Environment Enabling

To only apply a hook if `JUNIT_ENABLED=1`, wrap it in `whenEnabled`:

```
JUNIT_ENABLED=1
```

```haskell
hook5 :: Spec -> Spec
hook5 = FormatterEnv.whenEnabled FormatterEnv.add
```

### Without `hspec-discover`

Hooks are just functions of type `Spec -> Spec`, so you can apply them right
before calling `hspec` in `main`:

```haskell
main :: IO ()
main = hspec $ FormatterEnv.whenEnabled FormatterEnv.add spec

spec :: Spec
spec = describe "Addition" $ do
  it "adds" $ do
    2 + 2 `shouldBe` (4 :: Int)
```

## Golden Testing

This project's test suite uses [hspec-golden][] to generate an XML report for
[`ExampleSpec.hs`](./tests/ExampleSpec.hs) and then compare that with golden XML
files checked into the repository. If your work changes things in a
functionally-correct way, but that diverges from the golden XML files, you need
to regenerate them.

1. Run `rm tests/golden*.xml`
2. Run the specs again

We maintain specific golden XML files for GHC 8.x vs 9.x, so you will need to
re-run the test suite with at least one of each series to regenerate all the
necessary files.

---

[LICENSE](./LICENSE)
