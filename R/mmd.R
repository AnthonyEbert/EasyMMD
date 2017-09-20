


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
#' @param x_obs Numeric Vector
#' @param x_sim Numeric Vector
#' @param x_obs_kmmd Precomputed first term in MMD calculation.
#' @param a The squared norm of x_obs, see \link[kernlab]{kernelFast}
#' @param rbf The radial basis function, see \link[kernlab]{kernelMatrix}
#' @param bias Logical Should the biased or unbiased MMD be computed?
#' @export
#' @description This function returns the estimator for the two-sample MMD.
#' @references Gretton, Arthur, et al. "A kernel method for the two-sample-problem." Advances in neural information processing systems. 2007.
#' @examples
#' x_obs <- rnorm(2000)
#' x_sim <- rnorm(2000, 5)
#'
#' MMD_1 <- MMD(x_obs, x_sim)
#' MMD_1
#'
#' # Precompute x_obs_kmmd for faster speed
#'
#' rbf = kernlab::rbfdot(sigma = 1)
#' x_obs_kmmd <- sum(
#'      kernlab::kernelMatrix(
#'          rbf,as.matrix(x_obs),as.matrix(x_obs)
#'      )
#' )
#'
#' MMD_2 <- MMD(x_obs, x_sim, x_obs_kmmd)
#' MMD_2
#'
#' # Precompute the squared norm of x_obs for more speed!
#'
#' a <- rowSums(as.matrix(x_obs)^2)
#'
#' MMD_3 <- MMD(x_obs, x_sim, x_obs_kmmd, a)
#' MMD_3
#'
#' system.time(MMD_1 <- MMD(x_obs, x_sim))
#' system.time(MMD_2 <- MMD(x_obs, x_sim, x_obs_kmmd))
#' system.time(MMD_3 <- MMD(x_obs, x_sim, x_obs_kmmd, a))
#'
#' # Different radial basis function
#'
#' rbf = kernlab::rbfdot(sigma = 0.5)
#'
#' MMD_4 <- MMD(x_obs, x_sim, rbf = rbf)
MMD <- function(y, x, y_kmmd = NULL, sigma = 1, bias = FALSE){

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
    y_kmmd <- kernelMatrix_cpp(y, y, sigma = sigma)
  }

  term_xx <- (kernelMatrix_cpp(x, x, sigma = sigma) - bias_x)/denom_x
  term_yy <- (y_kmmd - bias_y)/denom_y
  term_xy <- kernelMatrix_cpp(x, y, sigma = sigma)/(n_x*n_y)

  output = term_xx + term_yy - 2*term_xy

  return(output)
}


#' @export
kmmd <- function(x, sigma = 1){
  return(kernelMatrix_cpp(x, x, sigma = 1))
}





