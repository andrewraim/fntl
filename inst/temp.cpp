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
	const Rcpp::NumericMatrix& out2 = fntl::as<double, REALSXP>(out_sp);
	const fntl::coo_mat<double>& out3 = fntl::as<double>(out_sp);

	return Rcpp::List::create(
		Rcpp::Named("ds") = out,
		Rcpp::Named("sp") = Rcpp::wrap(out_sp),
		Rcpp::Named("ds2") = out2,
		Rcpp::Named("sp2") = Rcpp::wrap(out3)
	);
}

/*
* library(Matrix)
* x = matrix(rnorm(12), 4, 3)
* y = matrix(rnorm(12), 4, 3)
* out = temp_ex(x, y)
* ds = out$ds
* sp = out$sp
* ds2 = out$ds2
* sp2 = out$sp2
* Matrix::sparseMatrix(i = sp$i, p = sp$p, x = sp$x, dims = c(sp$m, sp$n), index1 = FALSE)
* Matrix::sparseMatrix(i = sp2$i, j = sp2$j, x = sp2$x, dims = c(sp2$m, sp2$n), index1 = FALSE)
*/

