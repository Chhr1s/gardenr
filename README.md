
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{gardenr}`

<!-- badges: start -->

[![R-CMD-check](https://github.com/Chhr1s/gardenr/workflows/R-CMD-check/badge.svg)](https://github.com/Chhr1s/gardenr/actions)
[![Codecov test
coverage](https://codecov.io/gh/Chhr1s/gardenr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Chhr1s/gardenr?branch=main)
<!-- badges: end -->

The goal of `{gardenr}` is to provide tools for general linear mixed
effects model regression (GLMM) trees as implemented in the
`{glmertree}` package.

## Installation

You can install the development version of `{gardenr}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Chhr1s/gardenr")
```

# purpose

The purpose of the `{gardenr}` package is to improve workflows with
model-based recursive partitioning. This includes GLM trees, GLMM trees,
and will soon incorporate other variants. At present, this is mostly for
cross-validation improvements to tune hyperparameters, but this will
extend into novel pruning mechanisms.

Please see the vignette for examples.
