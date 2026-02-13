#ifndef FNTL_OPTIMIZE_BRENT_H
#define FNTL_OPTIMIZE_BRENT_H

#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "args.h"
#include "result.h"

/*
* A C++ implementation of procedure `localmin` from Section 5.8 of Brent (1973).
*
* Brent, R. P. 1973. Algorithms for Minimization Without Derivatives. Prentice-Hall.
*/

namespace fntl {

inline optimize_result optimize_brent(const dfd& f, double lower,
	double upper, const optimize_args& args)
{
	unsigned int maxiter = args.maxiter;
	error_action action = args.action;
	double tol0 = args.tol;
	double fnscale = args.fnscale;
	unsigned int report_period = args.report_period;

	double c = 0.5 * (3 - sqrt(5));

	double a = lower;
	double b = upper;

	double x = a + c * (b - a);
	double w = x;
	double v = x;
	double e = 0;

	double fx = fnscale * f(x);
	double fw = fx;
	double fv = fx;

	double m;
	double d;
	double u;
	double fu;
	unsigned int iter = 0;
	optimize_status status = optimize_status::NOT_CONVERGED;

	while (iter <= maxiter) {
		iter++;

		m = 0.5 * (a + b);
		double tol = mach_eps * fabs(x) + tol0;

		bool converged = fabs(x - m) <= 2*tol - 0.5 * (b - a);
		if (converged) {
			status = optimize_status::OK;
			break;
		}

		if (iter % report_period == 0) {
			Rprintf("iter %d  [%g, %g]  f(%g) = %g  err: %g\n",
				iter, a, b, x, fnscale*fx, std::fabs(x - m));
		}

		double p = 0;
		double q = 0;
		double r = 0;

		if (fabs(e) > tol) {
			// Fit parabola
			r = (x - w) * (fx - fv);
			q = (x - v) * (fx - fw);
			p = (x - v) * q - (x - w) * r;
			q = 2 * (q - r);
			if (q > 0) { p *= -1; } else { q *= -1; }
			r = e;
			e = d;
		}

		bool cond1 = fabs(p) < fabs(0.5 * q * r);
		bool cond2 = p < q * std::max(a - x, b - x);
		bool cond = cond1 && cond2;

		if (cond) {
			// Parabolic interpolation step
			d = p / q;
			u = x + d;
			double s = x < m ? tol: -tol;
			d = u - a < 2*tol || b - u < 2*tol ? s : d;
		} else {
			// Golden section step
			double s = x < m ? b : a;
			e = s - x;
			d = c * e;
		}

		// f must not be evaluated too close to x
		double delta = d > 0 ? tol : -tol;
		double s = fabs(d) >= tol ? d : delta;
		u = x + s;
		fu = fnscale * f(u);

		// Update a, b, v, w, and x
		if (fu <= fx) {
			if (u < x) { b = x; } else { a = x; }
			v = w;
			w = x;
			x = u;
			fv = fw;
			fw = fx;
			fx = fu;
		} else {
			if (u < x) { a = u; } else { b = u; }
			if (fu <= fx || w == x) {
				v = w;
				w = u;
				fv = fw;
				fw = fu;
			} else if (fu <= fv || v == x || v == w) {
				v = u;
				fv = fu;
			}
		}
	}

	const std::string& message = optimize_messages[static_cast<unsigned int>(status)];

	if (status != optimize_status::OK) {
		switch (action) {
			case error_action::STOP:
				Rcpp::stop(message.c_str());
				break;
			case error_action::WARNING:
				Rcpp::warning(message.c_str());
				break;
			case error_action::MESSAGE:
				Rprintf("%s\n", message.c_str());
				break;
			default:
				break;
		}
	}

	optimize_result out;
	out.par = x;
	out.value = fnscale * fx;
	out.iter = iter;
	out.tol = fabs(x - m);
	out.status = status;
	out.message = message;

	return out;
}

inline optimize_result optimize_brent(const dfd& f, double lower, double upper)
{
	optimize_args args;
	return optimize_brent(f, lower, upper, args);
}

}

#endif
