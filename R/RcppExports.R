# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

kernelMatrix_sum_multi <- function(x, y, w_x, w_y, Sinv, threshold) {
    .Call(`_EasyMMD_kernelMatrix_sum_multi`, x, y, w_x, w_y, Sinv, threshold)
}

kernelMatrix_sum <- function(x, y, w_x, w_y, sigma, approx_exp) {
    .Call(`_EasyMMD_kernelMatrix_sum`, x, y, w_x, w_y, sigma, approx_exp)
}

kernelMatrix_threshold_sum <- function(x_u, y_u, w_x, w_y, sigma, threshold, approx_exp) {
    .Call(`_EasyMMD_kernelMatrix_threshold_sum`, x_u, y_u, w_x, w_y, sigma, threshold, approx_exp)
}

