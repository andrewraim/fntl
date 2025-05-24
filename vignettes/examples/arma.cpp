// [[Rcpp::depends(fntl)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "fntl.h"

// [[Rcpp::export]]
double arma_example(arma::vec y, arma::vec mu)
{
    unsigned int n = y.n_elem;
    double qq = 0.5 * arma::dot(y - mu, y - mu);

    fntl::dfd f = [&](double sigma2) {
        return -(n / 2.0) * std::log(sigma2) - qq / sigma2;
    };

    fntl::optimize_args args;
    args.fnscale = -1;
    auto out = fntl::optimize_brent(f, 0, 100, args);
    return out.par;
}

