
#' Compute the MMD between two samples
#'
#' @useDynLib EasyMMD
#' @importFrom Rcpp sourceCpp
#' @param y either a numeric vector or matrix with number of rows equal to number of observations and number of columns equal to dimension of observations.
#' @param x either a numeric vector or matrix with number of rows equal to number of observations and number of columns equal to dimension of observations.
#' @param y_kmmd precomputed first term in MMD calculation.
#' @param var matrix kernel variance covariance matrix.
#' @param bias logical; if \code{TRUE} the biased MMD is computed rather than the unbiased MMD. This can be useful since the biased MMD is always positive.
#' @param threshold numeric filter out values for exponentiation.
#' @param approx_exp integer; if 0 the usual function for the exponential distribution is used; if 1 a much faster but less accurate version of the exponential distribution is used.
#' @param sigma numeric DEPRECATED square root of variance.
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
MMD <- function(y, x, y_kmmd = NULL, var = 1, bias = FALSE, threshold = Inf, approx_exp = 0, sigma = NULL){

  if(!is.null(sigma)){var = sigma^2}

  stopifnot(class(x) == class(y))

  kernsum <- function(y, x){

    return(kernelMatrix_sum_wrap(y, x, var = var, threshold = threshold, approx_exp = approx_exp))
  }

  if(!is.matrix(x)){
    n_x <- length(x)
    n_y <- length(y)
  } else {
    n_x <- dim(x)[1]
    n_y <- dim(y)[1]
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
    y_kmmd <- kernsum(y, y)
  }

  term_xx <- (kernsum(x, x) - bias_x)/denom_x
  term_yy <- (y_kmmd - bias_y)/denom_y
  term_xy <- kernsum(x, y)/(n_x*n_y)

  output = term_xx + term_yy - 2*term_xy

  return(output)
}

kernelMatrix_sum_wrap <- function(y, x, var = 1, threshold = Inf, approx_exp = 0){

  if(!is.matrix(x)){
    sigma = sqrt(var)

    if(is.infinite(threshold)){
        return(kernelMatrix_sum(y, x, sigma = sigma, approx_exp = approx_exp))
    } else {
        return(kernelMatrix_threshold_sum(y, x, sigma = sigma, threshold = threshold, approx_exp = approx_exp))
    }
  } else {

    stopifnot(dim(x)[2] == dim(y)[2])
    stopifnot(dim(sigma)[1] == dim(sigma)[2])
    stopifnot(dim(sigma)[1] == dim(x)[2])

    Sinv <- solve(var)

    return(kernelMatrix_sum_multi(y, x, Sinv = Sinv, threshold = threshold))
  }
}


#' Compute the kmmd for one sample \code{y}
#' @param y either a numeric vector or matrix with number of rows equal to number of observations and number of columns equal to dimension of observations.
#' @param var matrix kernel variance covariance matrix.
#' @param threshold numeric filter out values for exponentiation.
#' @param approx_exp integer; if 0 the usual function for the exponential distribution is used; if 1 a much faster but less accurate version of the exponential distribution is used.
#' @export
kmmd <- function(y, var = 1, threshold = Inf, approx_exp = 0){
  return(kernelMatrix_sum_wrap(y, y, var = var, threshold = Inf, approx_exp = 0))
}





