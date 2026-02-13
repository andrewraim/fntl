#ifndef FNTL_GOLDENSECTION_H
#define FNTL_GOLDENSECTION_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "args.h"
#include "result.h"

namespace fntl {

inline optimize_result goldensection(const dfd& f, double lower,
	double upper, const optimize_args& args)
{
	double tol = args.tol;
	unsigned int maxiter = args.maxiter;
	unsigned int report_period = args.report_period;
	error_action action = args.action;
	double fnscale = args.fnscale;

	if (upper < lower) {
		Rcpp::stop("upper < lower");
	}

	double f_lower = fnscale * f(lower);
	if (std::isnan(f_lower)) {
		Rcpp::stop("f(lower) = nan");
	}

	double f_upper = fnscale * f(upper);
	if (std::isnan(f(upper))) {
		Rcpp::stop("f(upper) = nan");
	}

	double ratio = 0.5 * (std::sqrt(5) + 1);
	double delta = std::fabs(upper - lower);
	unsigned int iter = 0;

	if (iter % report_period == 0 && report_period < uint_max) {
		Rprintf("%d  [%g, %g]  f(%g): %g  f(%g): %g\n", iter, lower, upper,
			lower, f_lower, upper, f_upper);
	}

	while (delta > tol && iter <= maxiter) {
		iter++;
		double lo0 = upper - (upper - lower) / ratio;
		double hi0 = lower + (upper - lower) / ratio;
		double f_lo0 = fnscale * f(lo0);
		double f_hi0 = fnscale * f(hi0);

		if (f_lo0 < f_hi0) {
			upper = hi0;
			f_upper = f_hi0;
		} else {
			lower = lo0;
			f_lower = f_lo0;
		}

		delta = std::fabs(upper - lower);

		if (iter % report_period == 0) {
			Rprintf("%d  [%g, %g]  f(%g): %g  f(%g): %g\n", iter, lower, upper,
				lower, f_lower, upper, f_upper);
		}
	}

	optimize_status status;
	if (upper < lower) {
		status = optimize_status::NUMERICAL_OVERFLOW;
	} else if (iter == maxiter && delta > tol) {
		status = optimize_status::NOT_CONVERGED;
	} else {
		status = optimize_status::OK;
	}

	const std::string& message = optimize_messages[static_cast<unsigned int>(status)];

	if (status != optimize_status::OK) {
		if (action == error_action::STOP) {
			Rcpp::stop(message.c_str());
		} else if (action == error_action::WARNING) {
			Rcpp::warning(message.c_str());
		} else if (action == error_action::MESSAGE) {
			Rprintf("%s\n", message.c_str());
		}
	}

	optimize_result out;
	out.par = (upper + lower) / 2;
	out.value = f(out.par);
	out.iter = iter;
	out.tol = delta;
	out.status = status;
	out.message = message;

    return out;
}

inline optimize_result goldensection(const dfd& f, double lower,
	double upper)
{
	optimize_args args;
	return goldensection(f, lower, upper, args);
}

}

#endif
