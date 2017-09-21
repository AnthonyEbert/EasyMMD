// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// kernelMatrix_sum
double kernelMatrix_sum(NumericVector x_obs, NumericVector x_sim, float sigma);
RcppExport SEXP _EasyMMD_kernelMatrix_sum(SEXP x_obsSEXP, SEXP x_simSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_obs(x_obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x_sim(x_simSEXP);
    Rcpp::traits::input_parameter< float >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_sum(x_obs, x_sim, sigma));
    return rcpp_result_gen;
END_RCPP
}
// kernelMatrix_threshold_sum
double kernelMatrix_threshold_sum(NumericVector x_obs, NumericVector x_sim, float sigma, float threshold);
RcppExport SEXP _EasyMMD_kernelMatrix_threshold_sum(SEXP x_obsSEXP, SEXP x_simSEXP, SEXP sigmaSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_obs(x_obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x_sim(x_simSEXP);
    Rcpp::traits::input_parameter< float >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< float >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_threshold_sum(x_obs, x_sim, sigma, threshold));
    return rcpp_result_gen;
END_RCPP
}
// kernelMatrix_linear_sum
double kernelMatrix_linear_sum(NumericVector x_obs, NumericVector x_sim, float sigma);
RcppExport SEXP _EasyMMD_kernelMatrix_linear_sum(SEXP x_obsSEXP, SEXP x_simSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_obs(x_obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x_sim(x_simSEXP);
    Rcpp::traits::input_parameter< float >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_linear_sum(x_obs, x_sim, sigma));
    return rcpp_result_gen;
END_RCPP
}
// kernelMatrix_poly_sum
double kernelMatrix_poly_sum(NumericVector x_obs, NumericVector x_sim, float sigma, int degree, float offset);
RcppExport SEXP _EasyMMD_kernelMatrix_poly_sum(SEXP x_obsSEXP, SEXP x_simSEXP, SEXP sigmaSEXP, SEXP degreeSEXP, SEXP offsetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_obs(x_obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x_sim(x_simSEXP);
    Rcpp::traits::input_parameter< float >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< int >::type degree(degreeSEXP);
    Rcpp::traits::input_parameter< float >::type offset(offsetSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_poly_sum(x_obs, x_sim, sigma, degree, offset));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_EasyMMD_kernelMatrix_sum", (DL_FUNC) &_EasyMMD_kernelMatrix_sum, 3},
    {"_EasyMMD_kernelMatrix_threshold_sum", (DL_FUNC) &_EasyMMD_kernelMatrix_threshold_sum, 4},
    {"_EasyMMD_kernelMatrix_linear_sum", (DL_FUNC) &_EasyMMD_kernelMatrix_linear_sum, 3},
    {"_EasyMMD_kernelMatrix_poly_sum", (DL_FUNC) &_EasyMMD_kernelMatrix_poly_sum, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_EasyMMD(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
