

#' Compute the MMD between two samples
#'
#' @param x_obs Numeric Vector
#' @param x_sim Numeric Vector
#' @param x_obs_kmmd Precomputed first term in MMD calculation.
#' @param a The squared norm of x_obs, see \link[kernlab]{kernelFast}
#' @param rbf The radial basis function, see \link[kernlab]{kernelMatrix}
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
MMD <- function(x_obs, x_sim, x_obs_kmmd = NULL, a = NULL, rbf = kernlab::rbfdot(sigma = 1)){

  stopifnot(is.numeric(x_obs) | is.matrix(x_obs))
  stopifnot(is.numeric(x_sim) | is.matrix(x_sim))
  stopifnot(class(rbf) == "rbfkernel")

  x_obs <- as.matrix(x_obs)
  x_sim <- as.matrix(x_sim)

  n_obs <- dim(x_obs)[1]
  n_sim <- dim(x_sim)[1]

  if(is.null(x_obs_kmmd)){

    x_obs_kmmd <- sum(kernlab::kernelMatrix(rbf,as.matrix(x_obs),as.matrix(x_obs)))

  }

  if(is.null(a)){

    a <- rowSums(as.matrix(x_obs)^2)

  }

  # Checks after these objects definitely exist
  stopifnot(length(x_obs_kmmd) == 1 & is.numeric(x_obs_kmmd))
  stopifnot(is.numeric(a))

  x_sim_kmmd <- sum(kernlab::kernelMatrix(rbf,as.matrix(x_sim),as.matrix(x_sim)))

  first_term <- x_obs_kmmd/(n_obs^2)
  second_term <- sum(kernlab::kernelFast(rbf,matrix(x_obs),matrix(x_sim), a))/(n_obs*n_sim)
  third_term <- x_sim_kmmd/(n_sim^2)

  output <- first_term - 2 * second_term + third_term

  return(output)

}


