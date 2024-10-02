#ifndef FNTL_FD_JACOBIAN_H
#define FNTL_FD_JACOBIAN_H

#include <Rcpp.h>
#include "gradient.h"
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "result.h"

namespace fntl {

inline jacobian_result jacobian(const vfv& f,
	const Rcpp::NumericVector& x, const richardson_args& args,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	const Rcpp::NumericVector& fx = f(x);
	unsigned int m = fx.size();
	unsigned int n = x.size();

	jacobian_result out;
	out.rows = m;
	out.cols = n;

	for (unsigned int i = 0; i < m; i++) {
		// The i-th component of f(x).
	    const dfv& f_i =
	    [&](const Rcpp::NumericVector& par) {
	    	return f(par)(i);
		};

		const gradient_result& grad_out = gradient(f_i, x, args, fd_type);

		out.value.insert(out.value.end(), grad_out.value.begin(), grad_out.value.end());
		out.err.insert(out.err.end(), grad_out.err.begin(), grad_out.err.end());
		out.iter.insert(out.iter.end(), grad_out.iter.begin(), grad_out.iter.end());
	}

    return out;
}

inline jacobian_result jacobian(const vfv& f, const Rcpp::NumericVector& x,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	richardson_args args;
	return jacobian(f, x, args, fd_type);
}

}

#endif
