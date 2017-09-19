
#' Compute the permuted Jack-knife MMD_linear
#' @param x_obs Numeric Vector
#' @param x_sim Numeric Vector
#' @param sigma Numeric Kernel size
#' @param agg Logical Should output be aggregated by mean?
#' @export
MMD_l_multi <- function(x_obs, x_sim, k, sigma = 1, agg = TRUE){
  output <- rep(NA, k)
  n <- length(x_obs)
  stopifnot(n == length(x_sim))

  for(i in 1:k){
    output[i] <- MMD_l(x_obs, x_sim, sigma = sigma, permute = TRUE)
  }

  if(agg){
    output <- mean(output)
  }

  return(output)
}


#' @export
MMD_test <- function(x){
  MMD_l_cpp(x, c(1:3), sigma = 1, TRUE)
}
