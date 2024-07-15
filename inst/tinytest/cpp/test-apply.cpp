// [[Rcpp::depends(fntl)]]
#include "fntl.h"

/*
* For vectors, we can use the sapply function in Rcpp.
*/
// [[Rcpp::export]]
Rcpp::NumericVector vec_pow(const Rcpp::NumericVector& x, double exponent = 2)
{
	std::function<double(double)> f =
	[&](double x) {
		return std::pow(x, exponent);
	};

	return Rcpp::sapply(x, f);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix mat_pow(const Rcpp::NumericMatrix& x, double exponent = 2)
{
	std::function<double(double)> f =
	[&](double x) {
		return std::pow(x, exponent);
	};

	return fntl::mat_apply(x, f);
}

// [[Rcpp::export]]
Rcpp::NumericVector row_sum(const Rcpp::NumericMatrix& x)
{
	std::function<double(const Rcpp::NumericVector&)> f =
	[&](const Rcpp::NumericVector& x) {
		return Rcpp::sum(x);
	};

	return fntl::row_apply(x, f);
}

// [[Rcpp::export]]
Rcpp::NumericVector col_sum(const Rcpp::NumericMatrix& x)
{
	std::function<double(const Rcpp::NumericVector&)> f =
	[&](const Rcpp::NumericVector& x) {
		return Rcpp::sum(x);
	};

	return fntl::col_apply(x, f);
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix imat_square(const Rcpp::IntegerMatrix& x)
{
	std::function<int(int)> f =
	[&](int x) {
		return x * x;
	};

	return fntl::mat_apply(x, f);
}
