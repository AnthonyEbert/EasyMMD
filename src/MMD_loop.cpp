

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace std;
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector MMD_l_cpp(NumericVector x_obs, NumericVector x_sim, NumericVector sigma, bool permute) {

  vec x_obs_vec = as<vec>(x_obs);
  vec x_sim_vec = as<vec>(x_sim);
  int m2 = x_obs_vec.size();
  vec seq2i_1 = linspace(0, m2 - 1, 1);
  vec seq2i = linspace(m2, 2*m2 - 1, 1);

  if(permute){
    x_obs_vec = shuffle(x_obs_vec);
    x_sim_vec = shuffle(x_sim_vec);

    //vec x_2i_1 = x_sim_vec[seq2i_1];
  }


  return(wrap(x_obs_vec));
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

// [[Rcpp::export]]
NumericVector shuffle_arma(NumericVector x){
  vec x_vec = as<vec>(x);
  x_vec = shuffle(x_vec);

  return(wrap(x_vec));
}
