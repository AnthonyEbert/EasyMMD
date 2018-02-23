
#' Gram matrix
#' @export
MMD_g <- function(y, x){

  sigma <- 1
  threshold <- 100

  output <- kernelMatrix_threshold(y, x, sigma, threshold)

  return(output)
}
