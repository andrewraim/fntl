#ifndef FNTL_FD2_DERIV_H
#define FNTL_FD2_DERIV_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "result.h"
#include "richardson.h"

namespace fntl {

inline double fd_deriv2(const dfv& f, const Rcpp::NumericVector& x,
	unsigned int i, unsigned int j, double h_i, double h_j,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	unsigned int n = x.size();
	if (i > n-1 || j > n-1) {
		Rcpp::stop("i and j must be between 0 and n-1");
	}

	double num;
	double den;

	Rcpp::NumericVector x1(x.begin(), x.end());
	Rcpp::NumericVector x2(x.begin(), x.end());
	Rcpp::NumericVector x3(x.begin(), x.end());
	Rcpp::NumericVector x4(x.begin(), x.end());

	switch (fd_type) {
		case fd_types::SYMMETRIC:
			x1(i) += h_i;
			x1(j) += h_j;
			x2(i) += h_i;
			x2(j) -= h_j;
			x3(i) -= h_i;
			x3(j) += h_j;
			x4(i) -= h_i;
			x4(j) -= h_j;
			den = 4*h_i*h_j;
			break;
		case fd_types::FORWARD:
			x1(i) += h_i;
			x1(j) += h_j;
			x2(i) += h_i;
			x2(j) += 0;
			x3(i) += 0;
			x3(j) += h_j;
			x4(i) += 0;
			x4(j) += 0;
			den = h_i*h_j;
			break;
		case fd_types::BACKWARD:
			x1(i) -= 0;
			x1(j) -= 0;
			x2(i) -= h_i;
			x2(j) -= 0;
			x3(i) -= 0;
			x3(j) -= h_j;
			x4(i) -= h_i;
			x4(j) -= h_j;
			den = h_i*h_j;
			break;
		default:
			Rcpp::stop("Unrecognized value of fd_type");
	}

	num = f(x1) - f(x2) - f(x3) + f(x4);
	return num / den;
}

inline richardson_result deriv2(const dfv& f,
	const Rcpp::NumericVector& x, unsigned int i, unsigned int j,
	const richardson_args& args, const fd_types& fd_type = fd_types::SYMMETRIC)
{
	const dfd& ff = [&](double h) {
		return fd_deriv2(f, x, i, j, h, h, fd_type);
	};

	return richardson(ff, args);
}

inline richardson_result deriv2(const dfv& f,
	const Rcpp::NumericVector& x, unsigned int i, unsigned int j,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	richardson_args args;
	return deriv2(f, x, i, j, args, fd_type);
}

}

#endif

