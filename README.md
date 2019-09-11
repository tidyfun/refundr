
<!-- README.md is generated from README.Rmd. Please edit that file -->

# refunder

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/tidyfun/refunder.svg?branch=master)](https://travis-ci.org/tidyfun/refunder)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/tidyfun/refunder?branch=master&svg=true)](https://ci.appveyor.com/project/tidyfun/refunder)
<!-- badges: end -->

The goal of **`refunder`** is to interface **`refund`** and
**`tidyfun`**.

## Installation

``` r
devtools::install_github("tidyfun/refunder")
```

## `tidy_fpca()`

The `tidy_fpca()` function peforms `fpca` using functions from the
`refund` package. The input is a dataframe with `tfd` columns.

Below are examples using the dti and cd4 datasets.

``` r
library(tidyfun)
library(refunder)

data(dti_df)
dti_fpca = tidy_fpca(cca, dti_df)

cd4_df = tibble(cd4_count = tfd(refund::cd4))
results = tidy_fpca(cd4_count, cd4_df, npc = 4)
```
