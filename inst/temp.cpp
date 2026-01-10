// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List temp_ex(const Rcpp::NumericMatrix& x, const Rcpp::NumericMatrix& y)
{
	const fntl::dfvv& f =
	[](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) -> double {
		return Rcpp::sum( (x - y) * (x - y) );
	};

	const fntl::bfvv& g =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) -> double {
		return fabs(f(x,y)) < 5;
	};

	const Rcpp::NumericMatrix& out = fntl::outer(x, y, f);
	const fntl::csc_dmat& out_sp = fntl::outer_sp(x, y, f, g);

	return Rcpp::List::create(
		Rcpp::Named("ds") = out,
		Rcpp::Named("sp") = Rcpp::wrap(out_sp)
	);
}

/*
* library(Matrix)
* out = temp_ex(x, y)
* ds = out$ds
* sp = out$sp
* Matrix::sparseMatrix(i = sp$i, p = sp$p, x = sp$x, dims = c(sp$m, sp$n), index1 = FALSE)
*/

