


MMD_oldR <-
  function(x_obs,
           x_sim,
           x_obs_kmmd = NULL,
           a = NULL,
           rbf = kernlab::rbfdot(sigma = 1),
           bias = FALSE) {

  stopifnot(is.numeric(x_obs) | is.matrix(x_obs))
  stopifnot(is.numeric(x_sim) | is.matrix(x_sim))
  stopifnot(class(rbf) == "rbfkernel")

  x_obs <- as.matrix(x_obs)
  x_sim <- as.matrix(x_sim)

  n_obs <- dim(x_obs)[1]
  n_sim <- dim(x_sim)[1]

  if(bias){
    denom_obs = n_obs^2
    denom_sim = n_sim^2
    bias_obs  = 0
    bias_sim  = 0
  } else {
    denom_obs = n_obs * (n_obs - 1)
    denom_sim = n_sim * (n_sim - 1)
    bias_obs  = diag(n_obs)
    bias_sim  = diag(n_sim)
  }

  if(is.null(x_obs_kmmd)){

    x_obs_kmmd <- sum(kernlab::kernelMatrix(rbf,as.matrix(x_obs),as.matrix(x_obs)) - bias_obs)

  }

  if(is.null(a)){

    a <- rowSums(as.matrix(x_obs)^2)

  }

  # Checks after these objects definitely exist
  stopifnot(length(x_obs_kmmd) == 1 & is.numeric(x_obs_kmmd))
  stopifnot(is.numeric(a))

  x_sim_kmmd <- sum(kernlab::kernelMatrix(rbf,as.matrix(x_sim),as.matrix(x_sim)) - bias_sim)

  first_term <- x_obs_kmmd/denom_obs
  second_term <- sum(kernlab::kernelFast(rbf,matrix(x_obs),matrix(x_sim), a))/(n_obs*n_sim)
  third_term <- x_sim_kmmd/denom_sim

  output <- first_term - 2 * second_term + third_term

  return(output)

}


#' Compute the MMD between two samples
#'
#' @useDynLib EasyMMD
#' @importFrom Rcpp sourceCpp
#' @param y Numeric Vector
#' @param x Numeric Vector
#' @param y_kmmd Precomputed first term in MMD calculation.
#' @param sigma Numeric Kernel size
#' @param bias Logical Should the biased or unbiased MMD be computed?
#' @param threshold Filter out values for exponentiation
#' @export
#' @description This function returns the estimator for the two-sample MMD.
#' @references Gretton, Arthur, et al. "A kernel method for the two-sample-problem." Advances in neural information processing systems. 2007.
#' @examples
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
MMD <- function(y, x, y_kmmd = NULL, sigma = 1, bias = FALSE, threshold = Inf){

  if(is.infinite(threshold)){
    kernsum <- function(...){
      kernelMatrix_sum(
        sigma = sigma,
        ...
      )
    }
  } else {
    kernsum <- function(...){
      kernelMatrix_threshold_sum(
        sigma = sigma,
        threshold = threshold,
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


#' @export
kmmd <- function(x, sigma = 1){
  return(kernelMatrix_sum(x, x, sigma = 1))
}





