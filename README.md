
[![Build Status](https://travis-ci.org/AnthonyEbert/EasyMMD.svg?branch=master)](https://travis-ci.org/AnthonyEbert/EasyMMD) [![codecov](https://codecov.io/gh/AnthonyEbert/queuecomputer/branch/master/graph/badge.svg)](https://codecov.io/gh/AnthonyEbert/queuecomputer)

<!-- README.md is generated from README.Rmd. Please edit that file -->
EasyMMD
=======

Gretton et al. (2007) introduced Maximum Mean Discrepancy (MMD). EasyMMD in an R package which provides a simple (hopefully!) and computationally efficient way to compute the MMD between two datasets.

Installation
------------

You can install EasyMMD from github with:

``` r
devtools::install_github("AnthonyEbert/EasyMMD")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(EasyMMD)

n <- 1e3
x <- rnorm(n)
y <- rnorm(n, 5)


MMD(y,x)
```

    ## [1] 1.163502

``` r
# For faster computation, precompute part of the MMD for the observed data y

y_kmmd <- kmmd(y)

MMD(y,x, y_kmmd)
```

    ## [1] 1.163502

``` r
microbenchmark::microbenchmark(MMD(y,x), MMD(y,x, y_kmmd), times = 10)
```

    ## Unit: milliseconds
    ##               expr      min       lq     mean   median       uq      max
    ##          MMD(y, x) 50.64673 50.67129 51.51340 51.19359 51.58215 54.11431
    ##  MMD(y, x, y_kmmd) 33.76800 33.78736 34.34702 34.01918 34.66659 36.21584
    ##  neval cld
    ##     10   b
    ##     10  a

References
----------

Gretton, A., Borgwardt, K. M., Rasch, M., Schölkopf, B., & Smola, A. J. (2007). A kernel method for the two-sample-problem. In Advances in neural information processing systems (pp. 513-520).
