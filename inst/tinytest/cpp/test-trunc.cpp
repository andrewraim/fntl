// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::NumericVector d_beta_trunc(Rcpp::NumericVector x, double shape1,
	double shape2, double lo, double hi, bool log = false)
{
    fntl::density f = [&](double x, bool log) {
        return R::dbeta(x, shape1, shape2, log);
    };

    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::pbeta(x, shape1, shape2, lower, log);
    };

    unsigned int n = x.size();
    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::d_trunc(x, lo_vec, hi_vec, f, F, log);
}

// [[Rcpp::export]]
Rcpp::NumericVector p_beta_trunc(Rcpp::NumericVector x, double shape1,
	double shape2, double lo, double hi, bool lower = true, bool log = false)
{
    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::pbeta(x, shape1, shape2, lower, log);
    };

    unsigned int n = x.size();
    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::p_trunc(x, lo_vec, hi_vec, F, lower, log);
}

// [[Rcpp::export]]
Rcpp::NumericVector q_beta_trunc(Rcpp::NumericVector p, double shape1,
	double shape2, double lo, double hi, bool lower = true, bool log = false)
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
    return fntl::q_trunc(p, lo_vec, hi_vec, F, Finv, lower, log);
}

// [[Rcpp::export]]
Rcpp::NumericVector r_beta_trunc(unsigned int n, double shape1, double shape2,
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






// [[Rcpp::export]]
Rcpp::NumericVector d_pois_trunc(Rcpp::NumericVector x, double lambda,
	double lo, double hi, bool log = false)
{
    fntl::density f = [&](double x, bool log) {
        return R::dpois(x, lambda, log);
    };

    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::ppois(x, lambda, lower, log);
    };

    unsigned int n = x.size();
    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::d_trunc(x, lo_vec, hi_vec, f, F, log);
}

// [[Rcpp::export]]
Rcpp::NumericVector p_pois_trunc(Rcpp::NumericVector x, double lambda,
	double lo, double hi, bool lower = true, bool log = false)
{
    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::ppois(x, lambda, lower, log);
    };

    unsigned int n = x.size();
    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::p_trunc(x, lo_vec, hi_vec, F, lower, log);
}

// [[Rcpp::export]]
Rcpp::NumericVector q_pois_trunc(Rcpp::NumericVector p, double lambda,
	double lo, double hi, bool lower = true, bool log = false)
{
    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::ppois(x, lambda, lower, log);
    };

    fntl::quantile Finv = [&](double x, bool lower, bool log) {
        return R::qpois(x, lambda, lower, log);
    };

    unsigned int n = p.size();
    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::q_trunc(p, lo_vec, hi_vec, F, Finv, lower, log);
}

// [[Rcpp::export]]
Rcpp::NumericVector r_pois_trunc(unsigned int n, double lambda, double lo,
	double hi)
{
    fntl::cdf F = [&](double x, bool lower, bool log) {
        return R::ppois(x, lambda, lower, log);
    };

    fntl::quantile Finv = [&](double x, bool lower, bool log) {
        return R::qpois(x, lambda, lower, log);
    };

    auto lo_vec = Rcpp::rep(lo, n);
    auto hi_vec = Rcpp::rep(hi, n);
    return fntl::r_trunc(n, lo_vec, hi_vec, F, Finv);
}
