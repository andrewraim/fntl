// [[Rcpp::depends(fntl,RcppArmadillo)]]
#include "RcppArmadillo.h"
#include "fntl.h"

// [[Rcpp::export]]
arma::sp_mat outer_sp_ex(Rcpp::NumericMatrix X)
{
    fntl::dfvv f = [](Rcpp::NumericVector x, Rcpp::NumericVector y) {
        double norm2 = Rcpp::sum(Rcpp::pow(x - y, 2));
        return std::sqrt(norm2);
    };

    fntl::bfvv g = [&](Rcpp::NumericVector x, Rcpp::NumericVector y) {
        return f(x,y) < 2.0;
    };

    auto res = fntl::outer_sp(X, f, g);

    // Pack the vectors i and j into an arma matrix with two rows. Convert
    // vector x into an arma vector. These are used to construct an arma sparse
    // matrix.
    // unsigned int N = res.x.size();
    // arma::umat loc(2, N);
    // loc.row(0) = arma::conv_to<arma::urowvec>::from(res.i);
    // loc.row(1) = arma::conv_to<arma::urowvec>::from(res.p);
    // arma::vec val = arma::conv_to<arma::vec>::from(res.x);

    // Use notation &v[0] to convert STL vector v to array
    arma::uvec i = arma::conv_to<arma::uvec>::from(res.i);
    arma::uvec p = arma::conv_to<arma::uvec>::from(res.p);
    arma::vec x = arma::conv_to<arma::vec>::from(res.x);
    return arma::sp_mat(i, p, x, res.m, res.n);

    // return arma::sp_mat(loc, val, res.m, res.n);
}

