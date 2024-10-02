#ifndef FNTL_NLM_H
#define FNTL_NLM_H

#include <R_ext/Applic.h>
#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "args.h"
#include "gradient.h"
#include "result.h"

// This is for internal use only
class nlm_adapter
{
private:
	const fntl::dfv& _f;
	const fntl::vfv& _g;
	const fntl::mfv& _h;
	double _fnscale;

public:
	nlm_adapter(
		const fntl::dfv& f,
		const fntl::vfv& g,
		const fntl::mfv& h,
		double fnscale)
		: _f(f), _g(g), _h(h), _fnscale(fnscale)
	{
	}

	double get_fnscale() const { return _fnscale; }
	const fntl::dfv& get_f() const { return _f; }
	const fntl::vfv& get_g() const { return _g; }
	const fntl::mfv& get_h() const { return _h; }

	static void eval(int n, double* par, double* ff, void* ex) {
		const nlm_adapter* p = static_cast<const nlm_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::dfv& f = p->get_f();
		ff[0] = p->get_fnscale() * f(x);
	}

	static void grad(int n, double* par, double* gg, void* ex) {
		const nlm_adapter* p = static_cast<const nlm_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::vfv& g = p->get_g();
		const Rcpp::NumericVector& out = p->get_fnscale() * g(x);
		for (int i = 0; i < n; i++) {
			gg[i] = out(i);
		}
	}

	static void hess(int nr, int n, double* par, double* hh, void* ex) {
		const nlm_adapter* p = static_cast<const nlm_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::mfv& h = p->get_h();
		const Rcpp::NumericMatrix& out = p->get_fnscale() * h(x);
		for (int j = 0; j < n; j++) {
			for (int i = 0; i < nr; i++) {
				hh[i + j*nr] = out(i,j);
			}
		}
	}
};

namespace fntl {

inline nlm_result nlm(const Rcpp::NumericVector& init, const dfv& f,
	const vfv& g, const mfv& h, const nlm_args& args)
{
	nlm_result out;
	unsigned int nr = init.length();
	unsigned int n = init.length();

	// Set flags for whether gradient and Hessian are supplied. If Hessian is
	// not specified, set iexp flag to indicate that it is expensive to compute.
	int iagflg = !g ? 0 : 1;
	int iahflg = !h ? 0 : 1;
	int iexp = iahflg;

	double fxout;
	double* xin = new double[n];
	double* xout = new double[n];
	double* gxout = new double[n];
	double* a = new double[n*n];
	double* wrk = new double[8*n];

	for (unsigned int i = 0; i < n; i++) {
		xin[i] = init(i);
	}

	// Make non-const copies of f and g
	dfv ff = f;
	vfv gg = g;
	mfv hh = h;

	nlm_adapter adapter(ff, gg, hh, args.fnscale);

	// Initialize this the same way as in src/library/stats/R/nlm.R
	std::vector<int> status_codes = { 9, 1, 17 };
	int status_code = status_codes[args.print_level];

	int term_code;

	double* typsize = new double[n];
	if (args.typsize.size() > 0 && args.typsize.size() != n) {
		Rcpp::stop("Dimension of typsize is != n");
	} else if (args.typsize.size() > 0) {
		for (unsigned int i = 0; i < n; i++) {
			typsize[i] = args.typsize[i];
		}
	} else {
		for (unsigned int i = 0; i < n; i++) {
			typsize[i] = 1;
		}
	}

	double stepmax = args.stepmax;
	if (std::isinf(args.stepmax)) {
		Rcpp::NumericVector s(typsize, typsize + n);
		double tmp = 1000 * sqrt(Rcpp::sum(Rcpp::pow(init / s, 2)));
		stepmax = std::max(tmp, 1000.0);
	}

	/*
	* *** optif9 ***
	* Call optif9 in R's C interface for Newton-type algorithm.
	*
	* Arguments:
	* 1. int nr: row dimension of matrix [In]
	* 2. int n: dimension of problem [In]
	* 3. double *x: optimization variable [In/Out]
	* 4. fcn_p fcn: function f to optimize [In]
	* 5. fcn_p d1fcn: gradient of f [In]
	* 6. d2fcn_p d2fcn: Hessian of f [In]
	* 7. void *state: external data to pass to the objective function [In/Out]
	* 8. double *typsiz: vector with typical size for each component of x [In]
	* 9. double fscale: estimate of scale of objective function [In]
	* 10. int method: algorithm to use to solve minimization problem [In]
	*   - 1: line search
    *   - 2: double dogleg
    *   - 3: more-hebdon
	* 11. int iexp: indicator (1 or 0) of whether f is expensive to evaluate.
	*     If so, hessian will be evaluated by secant update.
	* 12. int *msg: status code [In/Out]
	*   - On input, set to a positive number to inhibit certain checks.
	*   - On output, negative values represents error codes and 0 is no error.
	* 13. int ndigit: number of "good digits" in f [In]
	* 14. int itnlim: maximum number of iterations [In]
	* 15. int iagflg: indicator (1 or 0) of whether an analytic gradient
	*     function is supplied [In].
	* 16. int iahflg: indicator (1 or 0) of whether an analytic hessian
	*     function is supplied [In].
	* 17. double dlt: radius of trust region [In]
	* 18. double gradtl: tolerance to terminate algorithm based on the distance
	*     of gradient to zero [In]
	* 19. double stepmx: maximum allowable step size [In]
	* 20. double steptl: tolerance to terminate algorithm based on relative
	*     step size of successive iterates [In]
	* 21. double *xpls: local minimum [Out]
	* 22. double *fpls: function value at solution [Out]
	* 23. double *gpls: gradient at solution [Out]
	* 24. int *itrmcd: termination code [Out]
	*     - 0: Perfect.
	*     - 1: Relative gradient close to zero.
	*     - 2: Successive iterates within tolerance
	*     - 3: Last global step failed to locate a point lower than x
	*     - 4: Iteration limit exceeded.
	*     - 5: Maximum step size exceeded 5 consecutive times.
	* 25. double *a: workspace for hessian and its cholesky decomposition
	*     [In/Out]
	* 26. double *wrk: workspace [In/Out]
	* 27. int *itncnt: iteration count [Out]
	*
	* Return: void
	*/

	optif9(
		nr,                // 1
		n,                 // 2
		xin,               // 3
		nlm_adapter::eval, // 4
		nlm_adapter::grad, // 5
		nlm_adapter::hess, // 6
		&adapter,          // 7
		typsize,           // 8
		args.fscale,       // 9
		args.method,       // 10
		iexp,              // 11
		&status_code,      // 12
		args.ndigit,       // 13
		args.iterlim,      // 14
		iagflg,            // 15
		iahflg,            // 16
		args.trust_radius, // 17
		args.gradtol,      // 18
		stepmax,           // 19
		args.steptol,      // 20
		xout,              // 21
		&fxout,            // 22
		gxout,             // 23
		&term_code,        // 24
		a,                 // 25
		wrk,               // 26
		&out.iterations    // 27
	);

	// Error messages adapted from function opterror in
	// /src/library/stats/src/optimize.c
	if (status_code == -1) {
		Rcpp::stop("Non-positive number of parameters in nlm");
	} else if (status_code == -2) {
		Rcpp::stop("nlm is inefficient for 1-d problems");
	} else if (status_code == -3) {
		Rcpp::stop("Invalid gradient tolerance in nlm");
	} else if (status_code == -4) {
		Rcpp::stop("Invalid iteration limit in nlm");
	} else if (status_code == -5) {
		Rcpp::stop("Minimization function has no good digits in nlm");
	} else if (status_code == -6) {
		Rcpp::stop("No analytic gradient to check in nlm!");
	} else if (status_code == -7) {
		Rcpp::stop("No analytic Hessian to check in nlm!");
	} else if (status_code == -21) {
		Rcpp::stop("Probable coding error in analytic gradient");
	} else if (status_code == -22) {
		Rcpp::stop("Probable coding error in analytic Hessian");
	} else if (status_code < 0) {
		Rcpp::stop("Unknown error message (%d) in nlm", status_code);
	}

	out.par.assign(xout, xout + n);
	out.grad.assign(gxout, gxout + n);
	out.status = nlm_status(term_code);
	out.estimate = args.fnscale * fxout;

	delete[] xin;
	delete[] xout;
	delete[] gxout;
	delete[] a;
	delete[] wrk;

	return out;
}

inline nlm_result nlm(const Rcpp::NumericVector& init, const dfv& f,
	const vfv& g, const nlm_args& args)
{
	mfv h;
	return nlm(init, f, g, h, args);
}

inline nlm_result nlm(const Rcpp::NumericVector& init, const dfv& f,
	const nlm_args& args)
{
	vfv g;
	mfv h;
	return nlm(init, f, g, h, args);
}

inline nlm_result nlm(const Rcpp::NumericVector& init, const dfv& f,
	const vfv& g, const mfv& h)
{
	nlm_args args;
	return nlm(init, f, g, h, args);
}

inline nlm_result nlm(const Rcpp::NumericVector& init, const dfv& f,
	const vfv& g)
{
	nlm_args args;
	mfv h;
	return nlm(init, f, g, h, args);
}

inline nlm_result nlm(const Rcpp::NumericVector& init, const dfv& f)
{
	nlm_args args;
	vfv g;
	mfv h;
	return nlm(init, f, g, h, args);
}

}

#endif
