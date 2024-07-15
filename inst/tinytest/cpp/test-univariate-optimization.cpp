// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List sine_goldensection(double lower, double upper)
{
    const fntl::dfd& f = [](double x) { return sin(x); };
	const auto& out = fntl::goldensection(f, lower, upper);
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List sine_brent(double lower, double upper)
{
    const fntl::dfd& f = [](double x) { return sin(x); };
	const auto& out = fntl::optimize_brent(f, lower, upper);
	return Rcpp::wrap(out);
}
