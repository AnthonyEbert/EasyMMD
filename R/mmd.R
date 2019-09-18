
#' Compute the MMD between two samples
#'
#' @useDynLib EasyMMD, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @param y either a numeric vector or matrix with number of rows equal to number of observations and number of columns equal to dimension of observations.
#' @param x either a numeric vector or matrix with number of rows equal to number of observations and number of columns equal to dimension of observations.
#' @param y_kmmd precomputed first term in MMD calculation.
#' @param var matrix kernel variance covariance matrix.
#' @param bias logical; if \code{TRUE} the biased MMD is computed rather than the unbiased MMD. This can be useful since the biased MMD is always positive.
#' @param threshold numeric filter out values for exponentiation.
#' @param approx_exp integer; if 0 the usual function for the exponential distribution is used; if 1 a much faster but less accurate version of the exponential distribution is used.
#' @param sigma numeric DEPRECATED square root of variance.
#' @param w_y numeric weights for y
#' @param w_x numeric weights for x
#' @export
#' @description This function returns the estimator for the two-sample MMD.
#' @references Gretton, Arthur, et al. "A kernel method for the two-sample-problem." Advances in neural information processing systems. 2007.
#' @examples
#' set.seed(1)
#' y <- rnorm(2000)
#' x <- rnorm(2000, 5)
#'
#' MMD_1 <- MMD(y, x)
#' MMD_1
#'
#' # Precompute y_kmmd for faster speed
#' y_kmmd <- kmmd(y)
#'
#' MMD_2 <- MMD(y, x, y_kmmd)
#' MMD_2
#'
#'
#' system.time(MMD_1 <- MMD(y, x))
#' system.time(MMD_2 <- MMD(y, x, y_kmmd))
#'
#' # Different var
#'
#' MMD_4 <- MMD(y, x, var = 0.25)
#'
#' # Matrix version
#' library(mvtnorm)
#'
#' x <- rmvnorm(100, c(0,0), diag(c(2,2)))
#' sigma <- matrix(c(2,1.5,1.5,2), ncol=2)
#' y <- rmvnorm(100, c(0,0), sigma = sigma)
#'
#' MMD(y, x, var = diag(c(0.5, 0.5)), bias = TRUE)
#'
MMD <- function(y, x, y_kmmd = NULL, var = 1, bias = FALSE, threshold = Inf, approx_exp = 0, sigma = NULL, w_y = NULL, w_x = NULL){

  if(!is.matrix(x)){
    if(is.null(w_y)){w_y = rep(1, length(y))}
    if(is.null(w_x)){w_x = rep(1, length(x))}

    stopifnot(length(w_y) == length(y))
    stopifnot(length(w_x) == length(x))
  } else {
    if(is.null(w_y)){w_y = rep(1, dim(y)[1])}
    if(is.null(w_x)){w_x = rep(1, dim(x)[1])}

    stopifnot(length(w_y) == dim(y)[1])
    stopifnot(length(w_x) == dim(x)[1])
  }

  n_x <- sum(w_x)
  n_y <- sum(w_y)

  if(!is.null(sigma)){var = sigma^2}

  stopifnot(class(x) == class(y))

  kernsum <- function(y, x, w_y, w_x){

    return(kernelMatrix_sum_wrap(y, x, w_y, w_x, var = var, threshold = threshold, approx_exp = approx_exp))
  }

  if(bias){
    denom_y = n_y^2
    denom_x = n_x^2
    bias_y  = 0
    bias_x  = 0
  } else {
    denom_y = n_y * (n_y - 1)
    denom_x = n_x * (n_x - 1)
    bias_y  = n_y
    bias_x  = n_x
  }

  if(is.null(y_kmmd)){
    y_kmmd <- kernsum(y, y, w_y, w_y)
  }

  term_xx <- (kernsum(x, x, w_x, w_x) - bias_x)/denom_x
  term_yy <- (y_kmmd - bias_y)/denom_y
  term_xy <- kernsum(x, y, w_x, w_y)/(n_x*n_y)

  output = term_xx + term_yy - 2*term_xy

  return(output)
}

kernelMatrix_sum_wrap <- function(y, x, w_y, w_x, var = 1, threshold = Inf, approx_exp = 0){

  if(!is.matrix(x)){

    sigma = sqrt(var)

    if(is.infinite(threshold)){
        return(kernelMatrix_sum(y, x, w_y, w_x, sigma = sigma, approx_exp = approx_exp))
    } else {
        return(kernelMatrix_threshold_sum(y, x, w_y, w_x, sigma = sigma, threshold = threshold, approx_exp = approx_exp))
    }
  } else {

    stopifnot(dim(x)[2] == dim(y)[2])
    stopifnot(dim(sigma)[1] == dim(sigma)[2])
    stopifnot(dim(sigma)[1] == dim(x)[2])

    Sinv <- solve(var)

    return(kernelMatrix_sum_multi(y, x, w_y, w_x, Sinv = Sinv, threshold = threshold))
  }
}


#' Compute the kmmd for one sample \code{y}
#' @param y either a numeric vector or matrix with number of rows equal to number of observations and number of columns equal to dimension of observations.
#' @param w_y numeric weights for y.
#' @param var matrix kernel variance covariance matrix.
#' @param threshold numeric filter out values for exponentiation.
#' @param approx_exp integer; if 0 the usual function for the exponential distribution is used; if 1 a much faster but less accurate version of the exponential distribution is used.
#' @export
kmmd <- function(y, w_y = NULL, var = 1, threshold = Inf, approx_exp = 0){

  if(!is.matrix(y)){
    if(is.null(w_y)){w_y = rep(1, length(y))}

    stopifnot(length(w_y) == length(y))
  } else {
    if(is.null(w_y)){w_y = rep(1, dim(y)[1])}

    stopifnot(length(w_y) == dim(y)[1])
  }

  return(kernelMatrix_sum_wrap(y, y, w_y, w_y, var = var, threshold = threshold, approx_exp = approx_exp))
}





