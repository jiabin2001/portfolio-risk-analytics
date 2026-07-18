# Contributing

Use a focused branch, add or update tests for behavioural changes, and run:

```r
source("scripts/bootstrap.R")
testthat::test_dir("tests/testthat")
```

Public changes must not include secrets, private data, large model objects, or generated
cache files. Risk measures use positive loss magnitudes throughout.
