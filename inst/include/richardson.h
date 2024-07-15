#ifndef FNTL_RICHARDSON_H
#define FNTL_RICHARDSON_H

#include "typedefs.h"
#include "args.h"
#include "result.h"

namespace fntl {

/*
* These functions are currently not exposed through fntl.h. We could consider
* doing so if we want to document and support usages other than numerical
* derivatives.
*/

inline richardson_result richardson(const dfd& f,
	const richardson_args& args)
{
	double delta = args.delta;
	unsigned int n = args.maxiter;
	double h = args.h;
	double tol = args.tol;
	double accuracy_factor = args.accuracy_factor;

	Rcpp::NumericMatrix A(n+1, n+1);
	double err = R_PosInf;

	richardson_result out;
	out.status = richardson_status::NOT_CONVERGED;
	out.value = R_NaN;
	out.err = R_PosInf;
	out.iter = 0;

	for (unsigned int m = 0; m <= n; m++) {
		// A[i,1] = f(delta^i * h)
		double h_adj = exp( m*log(delta) + log(h) );
		A(m,0) = f(h_adj);
	}

	// Refer to the last loop as iteration "zero". Save the value as the result
	// in case n = 0, and consider tol to be infinity at this point.
	out.value = A(0,0);

	for (unsigned int m = 1; m <= n; m++) {
		double delta_adj = delta * delta;
		out.iter++;

		for (unsigned int q = 1; q <= m; q++) {
			delta_adj *= delta;
			double num = A(m,q-1) - delta_adj * A(m-1,q-1);
			double den = 1 - delta_adj;
			A(m,q) = num / den;

			double err1 = std::abs(A(m,q) - A(m,q-1));
			double err2 = std::abs(A(m,q) - A(m-1,q-1));
			double err0 = std::max(err1, err2);

			if (err0 < err) {
				err = err0;
				out.value = A(m,q);
			}
		}

		double err3 = std::fabs(A(m,m) - A(m-1,m-1));
		if (err3 > accuracy_factor * err) {
			out.status = richardson_status::NUMERICAL_PRECISION;
			break;
		}

		if (err < tol) {
			break;
		}
	}

	out.err = err;

	if (err < tol) {
		out.status = richardson_status::OK;
	}

	return out;
}

inline richardson_result richardson(const dfd& f)
{
	richardson_args args;
	return richardson(f, args);
}

}

#endif

