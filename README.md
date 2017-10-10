
[![Build Status](https://travis-ci.org/AnthonyEbert/EasyMMD.svg?branch=master)](https://travis-ci.org/AnthonyEbert/EasyMMD)

<!-- README.md is generated from README.Rmd. Please edit that file -->
EasyMMD
=======

The goal of EasyMMD is to ...

Installation
------------

You can install EasyMMD from github with:

``` r
# install.packages("devtools")
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

    ## [1] 0.9076949

``` r
# For faster computation, precompute part of the MMD for the observed data y

y_kmmd <- kmmd(y)

MMD(y,x, y_kmmd)
```

    ## [1] 0.9076949

``` r
microbenchmark::microbenchmark(MMD(y,x), MMD(y,x, y_kmmd), times = 10)
```

    ## Unit: milliseconds
    ##               expr      min       lq     mean   median       uq      max
    ##          MMD(y, x) 44.51275 44.78667 44.92244 44.88081 45.01757 45.45446
    ##  MMD(y, x, y_kmmd) 29.74064 29.78153 30.42399 29.89612 30.55844 34.21259
    ##  neval
    ##     10
    ##     10
