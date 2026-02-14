#ifndef FNTL_FINDROOT_BISECT_H
#define FNTL_FINDROOT_BISECT_H

/*
* The C code for uniroot in R appears not to be exported for easy inclusion in
* external programs. (See src/library/stats/src/stats.h). Instead, we will
* provide a simple bisection method for root finding.
*
* This version does not attempt to expand the limits if f(lower) and f(upper)
* have the same sign (which is a feature in uniroot that can be accessed via
* the `extendInt` argument.
*/

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "args.h"
#include "result.h"

namespace fntl {

inline findroot_result findroot_bisect(const dfd& f, double lower, double upper,
	const findroot_args& args)
{
	double f_lower = f(lower);
	double f_upper = f(upper);

	if (lower >= upper) {
		Rcpp::stop("lower >= upper");
	}
	if (std::isnan(f_lower)) {
		f_lower = f(lower);
	}
	if (std::isnan(f_upper)) {
		f_upper = f(upper);
	}
	if (f_lower * f_upper > 0) {
		Rcpp::stop("f(lower) and f(upper) do not have opposite sign");
	}

	double tol = args.tol;
	unsigned int maxiter = args.maxiter;
	error_action action = args.action;

	unsigned int iter = 0;
	double x = (lower + upper) / 2.0;

	double dist = upper - lower;
	while (dist > tol && x > lower && x < upper && iter < maxiter) {
		// If no sign change occurs between f(x) and f(lower), assign x to
		// lower. Otherwise assign x to upper.
		bool ind = (f(x) >= 0) == (f(lower) >= 0);
		lower = ind*x + (!ind)*lower;
		upper = ind*upper + (!ind)*x;
		x = (lower + upper) / 2.0;
		dist = upper - lower;
		iter++;
	}

	findroot_status status;
	if (dist > tol && (upper <= lower)) {
		status = findroot_status::NUMERICAL_OVERFLOW;
	} else if (iter == maxiter && dist > tol) {
		status = findroot_status::NOT_CONVERGED;
	} else {
		status = findroot_status::OK;
	}

	const std::string& message = findroot_messages[static_cast<unsigned int>(status)];

	if (status != findroot_status::OK) {
		if (action == error_action::STOP) {
			Rcpp::stop(message.c_str());
		} else if (action == error_action::WARNING) {
			Rcpp::warning(message.c_str());
		} else if (action == error_action::MESSAGE) {
			Rprintf("%s\n", message.c_str());
		}
	}

	findroot_result out;
	out.root = x;
	out.f_root = f(x);
	out.iter = iter;
	out.tol = tol;
	out.status = status;
	out.message = message;
	return out;
}

inline findroot_result findroot_bisect(const dfd& f, double lower, double upper)
{
	findroot_args args;
	return findroot_bisect(f, lower, upper, args);
}

}

#endif
