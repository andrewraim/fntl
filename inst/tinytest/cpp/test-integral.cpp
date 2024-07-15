// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List normal_integral(double mu, double sigma2, double a, double b)
{
	const fntl::dfd& f =
	[&](double x) {
		return exp(-pow(x - mu, 2) / (2*sigma2));
	};

	const auto& out = fntl::integrate(f, a, b);
	return Rcpp::wrap(out);
}
