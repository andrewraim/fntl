#ifndef FNTL_ROOT_BRENT_H
#define FNTL_ROOT_BRENT_H

#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "args.h"
#include "result.h"

/*
* A C++ implementation of procedure `zero` from Section 4.6 of Brent (1973).
*
* Brent, R. P. 1973. Algorithms for Minimization Without Derivatives. Prentice-Hall.
*/

namespace fntl {

inline findroot_result findroot_brent(const dfd& f, double lower,
	double upper, const findroot_args& args)
{
	unsigned int maxiter = args.maxiter;
	error_action action = args.action;
	double tol0 = args.tol;
	unsigned int report_period = args.report_period;

	double a = lower;
	double b = upper;

	double fa = f(a);
	double fb = f(b);

	double c = a;
	double fc = fa;

	unsigned int iter = 0;
	double tol;
	double m = 0.5 * (c - b);
	double e = R_PosInf;
	double d = R_PosInf;

	findroot_status status = findroot_status::NOT_CONVERGED;

	for (; iter < maxiter; iter++) {

		if (fb * fc > 0) {
			c = a;
			fc = fa;
			e = b - a;
			d = b - a;
		}

		if (std::fabs(fc) < std::fabs(fb)) {
			a = b;
			b = c;
			c = a;
			fa = fb;
			fb = fc;
			fc = fa;
		}

		tol = 2 * mach_eps * std::fabs(b) + tol0;
		m = 0.5 * (c - b);

		if (std::fabs(m) <= tol || fb == 0) {
			status = findroot_status::OK;
			break;
		}

		if (iter % report_period == 0 && report_period < uint_max) {
			Rprintf("iter %d  a: %g  c: %g  f(x): %g  err: %g\n", iter, a,
				c, fb, m);
		}

		// See if a bisection is forced
		if (std::fabs(e) < tol || std::fabs(fa) <= std::fabs(fb)) {
			d = m;
			e = m;
		} else {
			double s = fb / fa;
			double p;
			double q;
			double r;

			if (a == c) {
				// Linear interpolation
				p = 2 * m * s;
				q = 1 - s;
			} else {
				// Inverse quadratic interpolation
				q = fa / fc;
				r = fb / fc;
				p = s * (2 * m * q * (q - r) - (b - a)*(r - 1));
				q = (q - 1) * (r - 1) * (s - 1);
			}

			if (p > 0) { q *= -1; } else { p *= -1; }
			s = e;
			e = d;

			double bdd1 = 3 * m * q - std::fabs(q * tol);
			double bdd2 = std::fabs(s * q);
			double bdd = std::min(bdd1, bdd2);

			if (2 * p < bdd) {
				d = p / q;
			} else {
				d = m;
				e = m;
			}
		}

		a = b;
		fa = fb;
		double delta0 = m > 0 ? tol : -tol;
		double delta = fabs(d) > tol ? d : delta0;
		b += delta;
		fb = f(b);
	}

	const std::string& message = findroot_messages[to_underlying(status)];

	if (status != findroot_status::OK) {
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

	findroot_result out;
	out.root = b;
	out.f_root = fb;
	out.iter = iter;
	out.tol = m;
	out.status = status;
	out.message = message;

	return out;
}

inline findroot_result findroot_brent(const dfd& f, double lower, double upper)
{
	findroot_args args;
	return findroot_brent(f, lower, upper, args);
}

}

#endif
