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
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) -> bool {
		return fabs(f(x,y)) < 5;
	};

	//const std::function<bool(const double&)>&h =
	std::function h = [&](const double& x) -> bool {
		return fabs(x) < 5;
	};

	const Rcpp::NumericMatrix& out = fntl::outer(x, y, f);
	const fntl::csc_dmat& out_sp = fntl::outer_sp(x, y, f, g);
	const fntl::csc_dmat& out0_sp = fntl::to_csc(out, h);

	const Rcpp::NumericMatrix& out2 = fntl::to_Matrix<double, REALSXP>(out_sp);
	const fntl::coo_mat<double>& out3 = fntl::to_coo<double>(out_sp);
	const fntl::csr_mat<double>& out4 = fntl::to_csr<double>(out_sp);

	return Rcpp::List::create(
		Rcpp::Named("ds") = out,
		Rcpp::Named("ds2") = out2,
		Rcpp::Named("sp0") = Rcpp::wrap(out0_sp),
		Rcpp::Named("sp1") = Rcpp::wrap(out_sp),
		Rcpp::Named("sp2") = Rcpp::wrap(out3),
		Rcpp::Named("sp3") = Rcpp::wrap(out4)
	);
}
