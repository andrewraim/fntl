// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List sine_goldensection(double lower, double upper, bool maximize)
{
	fntl::optimize_args args;
	args.fnscale = maximize ? -1 : 1;
	const fntl::dfd& f = [](double x) { return sin(x); };
	const auto& out = fntl::goldensection(f, lower, upper, args);
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List sine_brent(double lower, double upper, bool maximize)
{
	fntl::optimize_args args;
	args.fnscale = maximize ? -1 : 1;
	const fntl::dfd& f = [](double x) { return sin(x); };
	const auto& out = fntl::optimize_brent(f, lower, upper, args);
	return Rcpp::wrap(out);
}
