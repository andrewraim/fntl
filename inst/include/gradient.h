#ifndef FNTL_GRAD_H
#define FNTL_GRAD_H

#include <Rcpp.h>
#include "deriv.h"
#include "richardson.h"
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "result.h"

namespace fntl {

inline gradient_result gradient(const dfv& f,
	const Rcpp::NumericVector& x, const richardson_args& args,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	unsigned int n = x.size();

	gradient_result out;

	for (unsigned int i = 0; i < n; i++) {
		const richardson_result& deriv_out = deriv(f, x, i, args, fd_type);

		out.value.push_back(deriv_out.value);
		out.err.push_back(deriv_out.err);
		out.iter.push_back(deriv_out.iter);
	}

    return out;
}

inline gradient_result gradient(const dfv& f,
	const Rcpp::NumericVector& x, const fd_types& fd_type = fd_types::SYMMETRIC)
{
	richardson_args args;
	return gradient(f, x, args);
}

}

#endif
