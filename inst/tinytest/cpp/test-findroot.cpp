// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List poly_root_bisect(double a, double b, double c, double lower, double upper)
{
    const fntl::dfd& f =
    [&](double x) {
    	return a*std::pow(x, 2.0) + b*x + c;
	};

	const auto& out = fntl::findroot_bisect(f, lower, upper);
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List poly_root_brent(double a, double b, double c, double lower, double upper)
{
    const fntl::dfd& f =
    [&](double x) {
    	return a*std::pow(x, 2.0) + b*x + c;
	};

	const auto& out = fntl::findroot_brent(f, lower, upper);
	return Rcpp::wrap(out);
}
