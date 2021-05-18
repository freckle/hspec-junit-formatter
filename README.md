# hspec-junit-formatter

A `JUnit` XML runner/formatter for [`hspec`](http://hspec.github.io/).

```hs
main :: IO ()
main = runSpec specs config

config :: Config
config = defaultConfig
  { configFormat = Just $ junitFormat "test-results.xml" "my-tests"
  }
```
