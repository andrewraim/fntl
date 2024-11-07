// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::NumericVector q_trunc_ex(Rcpp::NumericVector p, double shape1,
	double shape2, double lo, double hi)
{
    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::pbeta(x, shape1, shape2, lower, log);
    };

    fntl::quantile Finv = [&](double x, bool lower, bool log) {
        return R::qbeta(x, shape1, shape2, lower, log);
    };

    unsigned int n = p.size();
    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::q_trunc(p, lo_vec, hi_vec, F, Finv);
}
