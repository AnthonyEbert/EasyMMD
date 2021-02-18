
[![Build Status](https://travis-ci.org/AnthonyEbert/EasyMMD.svg?branch=master)](https://travis-ci.org/AnthonyEbert/EasyMMD) [![codecov](https://codecov.io/gh/AnthonyEbert/EasyMMD/branch/master/graph/badge.svg)](https://codecov.io/gh/AnthonyEbert/EasyMMD)

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

    ## [1] 1.13224

``` r
# For faster computation, precompute part of the MMD for the observed data y

y_kmmd <- kmmd(y)

MMD(y,x, y_kmmd)
```

    ## [1] 1.13224

``` r
microbenchmark::microbenchmark(MMD(y,x), MMD(y,x, y_kmmd), times = 10)
```

    ## Unit: milliseconds
    ##               expr      min       lq     mean   median       uq      max
    ##          MMD(y, x) 50.76673 51.54796 52.34023 52.02015 52.48663 56.36530
    ##  MMD(y, x, y_kmmd) 34.03786 34.39718 34.76865 34.63879 35.00489 36.29393
    ##  neval cld
    ##     10   b
    ##     10  a

References
----------

Gretton, A., Borgwardt, K. M., Rasch, M., Schölkopf, B., & Smola, A. J. (2007). A kernel method for the two-sample-problem. In Advances in neural information processing systems (pp. 513-520). [pdf](https://papers.nips.cc/paper/2006/file/e9fb2eda3d9c55a0d89c98d6c54b5b3e-Paper.pdf)
