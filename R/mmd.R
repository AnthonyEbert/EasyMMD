

#' Compute the MMD between two samples
#'
#' @param x_obs Numeric Vector
#' @param x_sim Numeric Vector
#' @param x_obs_kmmd Precomputed first term in MMD calc
#' @param a Termy term for the term
#' @param rbf The radial basis function
#' @export
MMD <- function(x_obs, x_sim, x_obs_kmmd = NULL, a = NULL, rbf = kernlab::rbfdot(sigma = 0.5)){

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

  x_sim_kmmd <- sum(kernlab::kernelMatrix(rbf,as.matrix(x_sim),as.matrix(x_sim)))

  first_term <- x_obs_kmmd/n_obs^2
  second_term <- sum(kernlab::kernelFast(rbf,matrix(x_obs),matrix(x_sim), a))/(n_obs*n_sim)
  third_term <- x_sim_kmmd/n_sim^2

  output <- sqrt(first_term - 2 * second_term + third_term)

  return(output)

}


