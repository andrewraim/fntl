// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::NumericVector r_trunc_ex(unsigned int n, double shape1, double shape2,
	double lo, double hi)
{
    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::pbeta(x, shape1, shape2, lower, log);
    };

    fntl::quantile Finv = [&](double x, bool lower, bool log) {
        return R::qbeta(x, shape1, shape2, lower, log);
    };

    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::r_trunc(n, lo_vec, hi_vec, F, Finv);
}
