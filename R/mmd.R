
#' Compute the MMD between two samples
#'
#' @useDynLib EasyMMD
#' @importFrom Rcpp sourceCpp
#' @param y numeric vector.
#' @param x numeric vector.
#' @param y_kmmd precomputed first term in MMD calculation.
#' @param sigma numeric kernel standard deviation.
#' @param bias logical; if \code{TRUE} the biased MMD is computed rather than the unbiased MMD. This can be useful since the biased MMD is always positive.
#' @param threshold numeric filter out values for exponentiation.
#' @param approx_exp integer; if 0 the usual function for the exponential distribution is used; if 1 a much faster but less accurate version of the exponential distribution is used.
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
#' # Different sigma
#'
#' MMD_4 <- MMD(y, x, sigma = 0.5)
MMD <- function(y, x, y_kmmd = NULL, sigma = 1, bias = FALSE, threshold = Inf, approx_exp = 0){

  if(is.infinite(threshold)){
    kernsum <- function(...){
      kernelMatrix_sum(
        sigma = sigma,
        approx_exp = approx_exp,
        ...
      )
    }
  } else {
    kernsum <- function(...){
      kernelMatrix_threshold_sum(
        sigma = sigma,
        threshold = threshold,
        approx_exp = approx_exp,
        ...
      )
    }
  }

  n_x <- length(x)
  n_y <- length(y)

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


#' Compute the kmmd for one sample \code{y}
#' @param y numeric Vector
#' @param sigma numeric kernel standard deviation
#' @param threshold numeric filter out values for exponentiation.
#' @export
kmmd <- function(y, sigma = 1, threshold = Inf){
  if(is.infinite(threshold)){
    kernsum <- function(...){
      kernelMatrix_sum(
        sigma = sigma,
        approx_exp = 0,
        ...
      )
    }
  } else {
    kernsum <- function(...){
      kernelMatrix_threshold_sum(
        sigma = sigma,
        threshold = threshold,
        approx_exp = 0,
        ...
      )
    }
  }
  return(kernsum(y, y))
}





