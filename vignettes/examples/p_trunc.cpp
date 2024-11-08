// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::NumericVector p_trunc_ex(Rcpp::NumericVector x, double shape1,
	double shape2, double lo, double hi)
{
    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::pbeta(x, shape1, shape2, lower, log);
    };

    unsigned int n = x.size();
    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::p_trunc(x, lo_vec, hi_vec, F);
}
