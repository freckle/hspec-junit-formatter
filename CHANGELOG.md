## [_Unreleased_](https://github.com/freckle/hspec-junit-formatter/compare/v1.1.2.0...main)

## [v1.1.2.0](https://github.com/freckle/hspec-junit-formatter/compare/v1.1.1.0...v1.1.2.0)

- Replace the string `{base}` by the basename of the current directory
  (typically the package name) when reading any `JUNIT` environment variable
  values.

## [v1.1.1.0](https://github.com/freckle/hspec-junit-formatter/compare/v1.1.0.2...v1.1.1.0)

- Add `Test.Hspec.JUnit.Formatter{,Env}`, for use as a [spec hook](https://hspec.github.io/hspec-discover.html#spec-hooks)
- Drop support for `hspec < 0.10`

## [v1.1.0.2](https://github.com/freckle/hspec-junit-formatter/compare/v1.1.0.1...v1.1.0.2)

- Support GHCs 9.0 and 9.2

## [v1.1.0.1](https://github.com/freckle/hspec-junit-formatter/compare/v1.1.0.0...v1.1.0.1)

- Release with looser lower-bounds
- CI against older GHCs

## [v1.1.0.0](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.3.0...v1.1.0.0)

- Remove incorrectly-cased `Test.HSpec` modules

## [v1.0.3.0](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.2.2...v1.0.3.0)

- Add `hspecJUnit` and environment-variable configuration
- Fix `file` attribute not respecting source-path-prefix

## [v1.0.2.2](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.2.1...v1.0.2.2)

- Add `file` and `line` attributes in `testcase` nodes

## [v1.0.2.1](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.2.0...v1.0.2.1)

- Support for prefixing reported source paths (e.g. if in a monorepo)
- Ensure tests pass within unpacked release tarball

## [v1.0.2.0](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.1.0...v1.0.2.0)

- Create `Test.Hspec` module-space and deprecate misspelled `Test.HSpec` modules
- Introduce `configWithJUnit` and `JUnitConfig`

## [v1.0.1.0](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.0.4...v1.0.1.0)

- Format function can be used directly without `withConfig` or `runJUnitSpec`.
- Test case duration is now supported.
- Failure locations are listed for some result types.
- Timestamps in the resulting XML now display the start time of formatting.

## [v1.0.0.4](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.0.3...v1.0.0.4)

- Bring `base` bound back in to fix release

## [v1.0.0.3](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.0.2...v1.0.0.3)

- Remove dependencies upper bounds

## [v1.0.0.2](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.0.1...v1.0.0.2)

- Less restrictive upper bound on `base`

## [v1.0.0.1](https://github.com/freckle/hspec-junit-formatter/compare/v1.0.0.0...v1.0.0.1)

- Bump `base` dep

## [v1.0.0.0](https://github.com/freckle/hspec-junit-formatter/tree/v1.0.0.0)

Initial release.
