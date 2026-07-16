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

### Mixing Formatters

In addition to the formatter named `junit`, this library also registers
formatters named `{formatter}+junit` for every existing formatter. This allows
doing something like `--format progress+junit` to produce output with `progress`
while also producing a JUnit file though `junit`.

The `add` hook can be thought of like `--format checks+junit`, so this extra
registration allows for similar "add" behavior on top of non-default formatters.

Once registered, they can be seen in Hspec's `--help`:

```console
  -f NAME  --format=NAME           use a custom formatter; this can be one of
                                   junit, checks+junit, specdoc+junit,
                                   progress+junit, failed-examples+junit,
                                   silent+junit, checks, specdoc, progress,
                                   failed-examples or silent
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
main = hspec $ FormatterEnv.register spec -- or use, or add

spec :: Spec
spec = describe "Addition" $ do
  it "adds" $ do
    2 + 2 `shouldBe` (4 :: Int)
```

## Release

To trigger a release, merge a commit to `main` that follows [Conventional
Commits][]. In short,

- `fix:` to trigger a patch release
- `feat:` to trigger a minor release
- `<type>!:` or use a `BREAKING CHANGE:` footer to trigger a major release

We don't enforce conventional commits generally (though you are free do so),
it's only required if you want to trigger release.

[conventional commits]: https://www.conventionalcommits.org/en/v1.0.0/#summary

---

[LICENSE](./LICENSE)
