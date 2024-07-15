#ifndef FNTL_FD_DERIV_H
#define FNTL_FD_DERIV_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "result.h"
#include "richardson.h"

namespace fntl {

inline double fd_deriv(const dfv& f, const Rcpp::NumericVector& x,
	unsigned int i, double h, const fd_types& fd_type = fd_types::SYMMETRIC)
{
	unsigned int n = x.size();
	if (i > n-1) {
		Rcpp::stop("i must be between 0 and n-1");
	}

	Rcpp::NumericVector x1(x.begin(), x.end());
	Rcpp::NumericVector x2(x.begin(), x.end());

	double den;

	switch (fd_type) {
		case fd_types::SYMMETRIC:
			x1(i) += h;
			x2(i) -= h;
			den = 2*h;
			break;
		case fd_types::FORWARD:
			x1(i) += h;
			x2(i) += 0;
			den = h;
			break;
		case fd_types::BACKWARD:
			x1(i) -= 0;
			x2(i) -= h;
			den = h;
			break;
		default:
			Rcpp::stop("Unrecognized value of fd_type");
	}

	double num = f(x1) - f(x2);
	return num / den;
}

inline richardson_result deriv(const dfv& f, const Rcpp::NumericVector& x,
	unsigned int i, const richardson_args& args,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	const dfd& ff = [&](double h) {
		return fd_deriv(f, x, i, h, fd_type);
	};

	return richardson(ff, args);
}

inline richardson_result deriv(const dfv& f, const Rcpp::NumericVector& x,
	unsigned int i, const fd_types& fd_type = fd_types::SYMMETRIC)
{
	richardson_args args;
	return deriv(f, x, i, args, fd_type);
}

}

#endif

