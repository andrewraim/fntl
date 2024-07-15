// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List quadratic_hessian(const Rcpp::NumericVector& x0)
{
    const fntl::dfv& f =
    [](const Rcpp::NumericVector& x) -> double { return Rcpp::sum(x * x); };

	const auto& out = fntl::hessian(f, x0);
	return Rcpp::wrap(out);
}
