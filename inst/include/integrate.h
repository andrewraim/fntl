#ifndef FNTL_INTEGRATE_H
#define FNTL_INTEGRATE_H

#include <R_ext/Applic.h>
#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "args.h"
#include "result.h"

// This is for internal use only
class integrate_adapter
{
protected:
	const fntl::dfd& _f;

public:
	integrate_adapter(const fntl::dfd& f)
	: _f(f)
	{
	}

	const fntl::dfd& get_f() const { return _f; }

	static void eval(double *x, int n, void *ex) {
		const integrate_adapter* p = static_cast<const integrate_adapter*>(ex);
		const fntl::dfd& f = p->get_f();
		for (int i = 0; i < n; i++) {
			x[i] = f(x[i]);
		}
	}
};

namespace fntl {

/*
* Mimic the call from R's integrate function to the C functions Rdqags or
* Rdqagi. Those functions are based on dqags and dqagi in FORTRAN. See
* <https://www.netlib.org/quadpack>.
*/
inline integrate_result integrate(const dfd& f, double lower,
	double upper, const integrate_args& args)
{
    double abs_tol = args.abs_tol;
    double rel_tol = args.rel_tol;
	int limit = args.subdivisions;
	bool stop_on_error = args.stop_on_error;

	if (limit < 1L) {
		Rcpp::stop("invalid parameter values");
	}

	double rel_limit = std::max(50 * std::numeric_limits<double>::epsilon(), 0.5e-28);
	if (abs_tol <= 0 && rel_tol < rel_limit) {
		Rcpp::stop("invalid parameter values");
	}

	dfd ff = f;
	integrate_adapter adapter(ff);
	integrate_result out;

	int ierr;
	int lenw = 4 * limit;

	int* iwork = new int[limit];
	double* work = new double[lenw];

	if (std::isfinite(lower) && std::isfinite(upper)) {
		/*
		* *** Numerical integration for definite integrals ***
		* Call Rdqags in R's C interface.
		*
		* Arguments:
		*
		* 1. integr_fn: function to integrate [In]
		* 2. void *ex: external data [In]
		* 3. double *a: lower limit of integral [In]
		* 4. double *b: upper limit of integral [In]
		* 5. double *epsabs: absolute error tolerance [In]
		* 6. double *epsrel: relative error tolerance [In]
		* 7. double *result: result of integration [Out]
		* 8. double *abserr: absolute error [Out]
		* 9. int *neval: number of function evaluations [Out]
		* 10. int *ier: return code [Out]
		* 11. int *limit: dimension of iwork array [In]
		* 12. int *lenw: dimension of work array [Out]
		* 13. int *last: number of subintervals produced in the subdivision process [Out]
		* 14. int *iwork: integer work array [Out]
		* 15. double *work: double precision work array [Out]
		*
		* Return: void
		*/
		Rdqags(
			integrate_adapter::eval, // 1
			&adapter,                // 2
			&lower,                  // 3
			&upper,                  // 4
			&abs_tol,                // 5
			&rel_tol,                // 6
			&out.value,              // 7
			&out.abs_error,          // 8
			&out.n_eval,             // 9
			&ierr,                   // 10
			&limit,                  // 11
			&lenw,                   // 12
			&out.subdivisions,       // 13
			iwork,                   // 14
			work                     // 15
		);
	} else {
		// Case for indefinite integrals
		if (std::isnan(lower) || std::isnan(upper)) {
			Rcpp::stop("a limit is NA or NaN");
		}

		int inf;
		double bound;

		if (std::isfinite(lower)) {
			inf = 1L;
			bound = lower;
		} else if (std::isfinite(upper)) {
			inf = -1L;
			bound = upper;
		} else {
			inf = 2L;
			bound = 0.0;
		}

		/*
		* *** Numerical integration for indefinite integrals ***
		* Call Rdqagi in R's C interface.
		*
		* Arguments:
		*
		* 1. integr_fn: function to integrate [In]
		* 2. void *ex: external data [In]
		* 3. double *bound: bound of integral limit, if either is finite [In]
		* 4. int* inf: code indicating whether one of the limits is finite [In]
		* 5. double *epsabs: absolute error tolerance [In]
		* 6. double *epsrel: relative error tolerance [In]
		* 7. double *result: result of integration [Out]
		* 8. double *abserr: absolute error [Out]
		* 9. int *neval: number of function evaluations [Out]
		* 10. int *ier: return code [Out]
		* 11. int *limit: dimension of iwork array [In]
		* 12. int *lenw: dimension of work array [Out]
		* 13. int *last: number of subintervals produced in the subdivision process [Out]
		* 14. int *iwork: integer work array [Out]
		* 15. double *work: double precision work array [Out]
		*
		* Return: void
		*/
		Rdqagi(
			integrate_adapter::eval, // 1
			&adapter,                // 2
			&bound,                  // 3
			&inf,                    // 4
			&abs_tol,                // 5
			&rel_tol,                // 6
			&out.value,              // 7
			&out.abs_error,          // 8
			&out.n_eval,             // 9
			&ierr,                   // 10
			&limit,                  // 11
			&lenw,                   // 12
			&out.subdivisions,       // 13
			iwork,                   // 14
			work                     // 15
		);
	}

	out.status = integrate_status(ierr);
	out.message = integrate_messages[static_cast<unsigned int>(out.status)];

	if (out.status == integrate_status::INVALID_INPUT) {
		Rcpp::stop(out.message);
	}

	if (out.status > integrate_status::OK && stop_on_error) {
		Rcpp::stop(out.message);
	}

	delete[] iwork;
	delete[] work;

	return out;
}

inline integrate_result integrate(const dfd& f, double lower,
	double upper)
{
	integrate_args args;
	return integrate(f, lower, upper, args);
}

}

#endif
