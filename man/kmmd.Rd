% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mmd.R
\name{kmmd}
\alias{kmmd}
\title{Compute the kmmd for one sample \code{y}}
\usage{
kmmd(y, w_y = NULL, var = 1, threshold = Inf, approx_exp = 0)
}
\arguments{
\item{y}{either a numeric vector or matrix with number of rows equal to number of observations and number of columns equal to dimension of observations.}

\item{w_y}{numeric weights for y.}

\item{var}{matrix kernel variance covariance matrix.}

\item{threshold}{numeric filter out values for exponentiation.}

\item{approx_exp}{integer; if 0 the usual function for the exponential distribution is used; if 1 a much faster but less accurate version of the exponential distribution is used.}
}
\description{
Compute the kmmd for one sample \code{y}
}
