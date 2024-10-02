#ifndef FNTL_FD_DERIV_H
#define FNTL_FD_DERIV_H

#include <R_ext/Applic.h>
#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "result.h"
#include "richardson.h"

namespace fntl {

inline double fd_fwd(const dfd& f, double x, double h)
{
	double num = f(x + h) - f(x);
	return num / h;
}

inline double fd_bkwd(const dfd& f, double x, double h)
{
	double num = f(x) - f(x - h);
	return num / h;
}

inline double fd_symm(const dfd& f, double x, double h)
{
	double num = f(x + h) - f(x - h);
	double den = 2*h;
	return num / den;
}

inline richardson_result fd_deriv(const dfd& f, double x,
	const richardson_args& args, const fd_types& fd_type = fd_types::SYMMETRIC)
{
	dfd fd;

	if (fd_type == fd_types::SYMMETRIC) {
		fd = [&](double h) {
			double num = f(x + h) - f(x - h);
			double den = 2*h;
			return num / den;
		};
	} else if (fd_type == fd_types::FORWARD) {
		fd = [&](double h) {
			double num = f(x + h) - f(x);
			return num / h;
		};
	} else if (fd_type == fd_types::BACKWARD) {
		fd = [&](double h) {
			double num = f(x) - f(x - h);
			return num / h;
		};
	} else {
		Rcpp::stop("Unrecognized value of fd_type");
	}

	return richardson(fd, x, args);
}

inline richardson_result fd_deriv(const dfd& f, double x,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	richardson_args args;
	return fd_deriv(f, x, args, fd_type);
}

}

#endif
