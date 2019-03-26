# hspec-junit-formatter

A `JUnit` XML runner/formatter for [`hspec`](http://hspec.github.io/).

```hs
main :: IO ()
main = do
  config <- readConfig defaultConfig =<< getArgs
  spec `runJUnitSpec` ("/tmp/junit", "my-tests") $ config
```
