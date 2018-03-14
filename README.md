
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

    ## [1] 1.139035

``` r
# For faster computation, precompute part of the MMD for the observed data y

y_kmmd <- kmmd(y)

MMD(y,x, y_kmmd)
```

    ## [1] 1.139035

``` r
microbenchmark::microbenchmark(MMD(y,x), MMD(y,x, y_kmmd), times = 10)
```

    ## Unit: milliseconds
    ##               expr      min       lq     mean   median       uq      max
    ##          MMD(y, x) 50.37642 50.63704 51.08804 50.85923 51.11588 52.66734
    ##  MMD(y, x, y_kmmd) 33.73091 33.77430 34.43660 33.98259 34.32549 37.97717
    ##  neval cld
    ##     10   b
    ##     10  a

References
----------

Gretton, A., Borgwardt, K. M., Rasch, M., Schölkopf, B., & Smola, A. J. (2007). A kernel method for the two-sample-problem. In Advances in neural information processing systems (pp. 513-520).
