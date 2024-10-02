#ifndef FNTL_HESSIAN_H
#define FNTL_HESSIAN_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "args.h"
#include "result.h"
#include "deriv2.h"

namespace fntl {

inline hessian_result hessian(const dfv& f,
	const Rcpp::NumericVector& x, const richardson_args& args,
	const fd_types& fd_type = fd_types::SYMMETRIC)
{
	unsigned int n = x.size();

	hessian_result out;
	out.dim = n;

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = j; i < n; i++) {
		    const richardson_result& deriv_out = deriv2(f, x, i, j, args, fd_type);
			out.value.push_back(deriv_out.value);
			out.err.push_back(deriv_out.err);
			out.iter.push_back(deriv_out.iter);
		}
	}

    return out;
}

inline hessian_result hessian(const dfv& f,
	const Rcpp::NumericVector& x, const fd_types& fd_type = fd_types::SYMMETRIC)
{
	richardson_args args;
	return hessian(f, x, args, fd_type);
}

}

#endif
