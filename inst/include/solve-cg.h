#ifndef FNTL_SOLVE_CG_H
#define FNTL_SOLVE_CG_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "cg.h"
#include "result.h"

namespace fntl {

inline cg_result solve_cg(const vfv& l,
	const Rcpp::NumericVector& b, const Rcpp::NumericVector& init,
	const cg_args& args)
{
	unsigned int n = b.size();

	const Rcpp::NumericVector& fx0 = l(init);
	if (fx0.size() != n) {
		Rcpp::stop("Dimension mismatch");
	}

	const dfv& f = [&](const Rcpp::NumericVector& x) {
		double xtAx = Rcpp::sum(x * l(x));
		double btx = Rcpp::sum(b * x);
		double out = 0.5 * xtAx - btx;
		return out;
	};

	const vfv& g = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& out = l(x) - b;
		return out;
	};

	return cg(init, f, g, args);
}

inline cg_result solve_cg(const vfv& l, const Rcpp::NumericVector& b,
	const Rcpp::NumericVector& init)
{
	cg_args args;
	return solve_cg(l, b, init, args);
}

inline cg_result solve_cg(const vfv& l, const Rcpp::NumericVector& b)
{
	Rcpp::NumericVector init(b.size());
	init.fill(0);
	cg_args args;
	return solve_cg(l, b, init, args);
}

}

#endif
