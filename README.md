
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

## `rfr_fpca()`

The `rfr_fpca()` function peforms `fpca` using functions from the
`refund` package. The input is a `tfd` column.

Below are examples using the dti and chf datasets.

``` r

library(refunder)

# irregular data 
data(dti_df)
dti_fpca = rfr_fpca(dti_df$cca)

# regular data
data(chf_df)
dti_fpca = rfr_fpca(chf_df$activity)
```
