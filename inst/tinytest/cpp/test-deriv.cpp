// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List sine_deriv(const Rcpp::NumericVector& x0, unsigned int type)
{
	const fntl::dfv& f = [](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& out = sin(x);
		return out(0);
	};
	const auto& out = fntl::deriv(f, x0, 0, fntl::fd_types(type));
	return Rcpp::wrap(out);
}
