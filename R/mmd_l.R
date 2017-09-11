
#' Compute the MMD between two samples in linear time
#'
#' @param x_obs Numeric Vector
#' @param x_sim Numeric Vector
#' @param sigma Numeric Kernel size
#' @examples
#' x_obs <- rnorm(2000)
#' x_sim <- rnorm(2000, 5)
#'
#' MMD_full <- MMD(x_obs, x_sim)
#' MMD_linear <- MMD_l(x_obs, x_sim)
#'
#' MMD_full
#' MMD_linear
#'
#' @export
MMD_l <- function(x_obs, x_sim, sigma = 1){

  stopifnot(is.numeric(x_obs) | is.matrix(x_obs))
  stopifnot(is.numeric(x_sim) | is.matrix(x_sim))
  stopifnot(length(x_obs) == length(x_sim))

  m2 <- floor(length(x_obs)/2)

  x_2i_1 <- x_sim[seq.int(1L, 2*m2-1, 2L)]
  x_2i   <- x_sim[seq.int(2L, 2*m2, 2L)]

  y_2i_1 <- x_obs[seq.int(1L, 2*m2-1, 2L)]
  y_2i   <- x_obs[seq.int(2L, 2*m2, 2L)]

  kern_1 <- sum(exp(-sigma*(x_2i_1 - x_2i)^2))
  kern_2 <- sum(exp(-sigma*(y_2i_1 - y_2i)^2))
  kern_3 <- -sum(exp(-sigma*(x_2i_1 - y_2i)^2))
  kern_4 <- -sum(exp(-sigma*(y_2i_1 - x_2i)^2))

  output <- sum(kern_1, kern_2, kern_3, kern_4) / m2

  return(output)
}






