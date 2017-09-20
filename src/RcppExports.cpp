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
// kernelMatrix_poly_sum
double kernelMatrix_poly_sum(NumericVector x_obs, NumericVector x_sim, float sigma);
RcppExport SEXP _EasyMMD_kernelMatrix_poly_sum(SEXP x_obsSEXP, SEXP x_simSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_obs(x_obsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x_sim(x_simSEXP);
    Rcpp::traits::input_parameter< float >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(kernelMatrix_poly_sum(x_obs, x_sim, sigma));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_EasyMMD_kernelMatrix_sum", (DL_FUNC) &_EasyMMD_kernelMatrix_sum, 3},
    {"_EasyMMD_kernelMatrix_poly_sum", (DL_FUNC) &_EasyMMD_kernelMatrix_poly_sum, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_EasyMMD(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
