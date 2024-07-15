// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List quadratic_jacobian(const Rcpp::NumericVector& x0)
{
    const fntl::vfv& f =
    [](const Rcpp::NumericVector& x) {
    	return Rcpp::NumericVector::create(
    		2 * Rcpp::sum(x*x),
    		4 * Rcpp::sum(x*x)
    	);
	};

    const auto& out = fntl::jacobian(f, x0);
	return Rcpp::wrap(out);
}
