
<!-- badges: start -->

[![R-CMD-check](https://github.com/AnthonyEbert/EasyMMD/workflows/R-CMD-check/badge.svg)](https://github.com/AnthonyEbert/EasyMMD/actions)
[![codecov](https://codecov.io/gh/AnthonyEbert/EasyMMD/branch/master/graph/badge.svg)](https://codecov.io/gh/AnthonyEbert/EasyMMD)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# EasyMMD

Gretton et al. (2007) introduced Maximum Mean Discrepancy (MMD). EasyMMD
in an R package which provides a simple (hopefully!) and computationally
efficient way to compute the MMD between two datasets.

## Installation

You can install EasyMMD from github with:

``` r
devtools::install_github("AnthonyEbert/EasyMMD")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(EasyMMD)

n <- 1e3
x <- rnorm(n)
y <- rnorm(n, 5)


MMD(y,x)
```

    ## [1] 1.120101

``` r
# For faster computation, precompute part of the MMD for the observed data y

y_kmmd <- kmmd(y)

MMD(y,x, y_kmmd)
```

    ## [1] 1.120101

``` r
microbenchmark::microbenchmark(MMD(y,x), MMD(y,x, y_kmmd), times = 10)
```

    ## Unit: milliseconds
    ##               expr      min       lq      mean   median       uq      max neval
    ##          MMD(y, x) 132.8372 133.4394 135.27005 133.6568 134.5340 142.3326    10
    ##  MMD(y, x, y_kmmd)  84.7363  85.0482  86.53848  85.5797  87.7038  91.0809    10

## References

Gretton, A., Borgwardt, K. M., Rasch, M., Schölkopf, B., & Smola, A. J.
(2007). A kernel method for the two-sample-problem. In Advances in
neural information processing systems (pp. 513-520).
