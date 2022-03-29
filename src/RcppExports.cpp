// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// kernelMatrix_sum_multi
double kernelMatrix_sum_multi(const arma::mat& x, const arma::mat& y, const arma::vec& w_x, const arma::vec& w_y, const arma::mat Sinv, const double threshold);
RcppExport SEXP _EasyMMD_kernelMatrix_sum_multi(SEXP xSEXP, SEXP ySEXP, SEXP w_xSEXP, SEXP w_ySEXP, SEXP SinvSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w_x(w_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w_y(w_ySEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type Sinv(SinvSEXP);
    Rcpp::traits::input_parameter< const double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_sum_multi(x, y, w_x, w_y, Sinv, threshold));
    return rcpp_result_gen;
END_RCPP
}
// kernelMatrix_sum
double kernelMatrix_sum(const arma::vec& x, const arma::vec& y, const arma::vec& w_x, const arma::vec& w_y, const float sigma, int approx_exp);
RcppExport SEXP _EasyMMD_kernelMatrix_sum(SEXP xSEXP, SEXP ySEXP, SEXP w_xSEXP, SEXP w_ySEXP, SEXP sigmaSEXP, SEXP approx_expSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w_x(w_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w_y(w_ySEXP);
    Rcpp::traits::input_parameter< const float >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< int >::type approx_exp(approx_expSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_sum(x, y, w_x, w_y, sigma, approx_exp));
    return rcpp_result_gen;
END_RCPP
}
// kernelMatrix_threshold_sum
double kernelMatrix_threshold_sum(const arma::vec& x_u, const arma::vec& y_u, const arma::vec& w_x, const arma::vec& w_y, const float sigma, const float threshold, int approx_exp);
RcppExport SEXP _EasyMMD_kernelMatrix_threshold_sum(SEXP x_uSEXP, SEXP y_uSEXP, SEXP w_xSEXP, SEXP w_ySEXP, SEXP sigmaSEXP, SEXP thresholdSEXP, SEXP approx_expSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x_u(x_uSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y_u(y_uSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w_x(w_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type w_y(w_ySEXP);
    Rcpp::traits::input_parameter< const float >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< const float >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< int >::type approx_exp(approx_expSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_threshold_sum(x_u, y_u, w_x, w_y, sigma, threshold, approx_exp));
    return rcpp_result_gen;
END_RCPP
}
