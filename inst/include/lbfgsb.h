#ifndef FNTL_LBFGSB_H
#define FNTL_LBFGSB_H

#include <R_ext/Applic.h>
#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "args.h"
#include "gradient.h"
#include "result.h"

// This is for internal use only
class lbfgsb_adapter
{
private:
	const fntl::dfv& _f;
	const fntl::vfv& _g;
	double _fnscale;

public:
	lbfgsb_adapter(const fntl::dfv& f,
		const fntl::vfv& g,
		double fnscale)
		: _f(f), _g(g), _fnscale(fnscale)
	{
	}

	double get_fnscale() const { return _fnscale; }
	const fntl::dfv& get_f() const { return _f; }
	const fntl::vfv& get_g() const { return _g; }

	static double eval(int n, double* par, void* ex) {
		const lbfgsb_adapter* p = static_cast<const lbfgsb_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::dfv& f = p->get_f();
		return p->get_fnscale() * f(x);
	}

	static void grad(int n, double* par, double* gr, void* ex) {
		const lbfgsb_adapter* p = static_cast<const lbfgsb_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::vfv& g = p->get_g();
		const Rcpp::NumericVector& out = p->get_fnscale() * g(x);
		for (int i = 0; i < n; i++) {
			gr[i] = out(i);
		}
	}
};

namespace fntl {

inline lbfgsb_result lbfgsb(const Rcpp::NumericVector& init,
	const dfv& f, const vfv& g, const lbfgsb_args& args)
{
	lbfgsb_result out;
	unsigned int n = init.length();

	std::vector<double> lower = args.lower;
	std::vector<double> upper = args.upper;

	if (lower.size() == 0) { lower.assign(n, R_NegInf); }
	if (upper.size() == 0) { upper.assign(n, R_PosInf); }

	if (lower.size() != n) { Rcpp::stop("Dimension mismatch for lower"); }
	if (upper.size() != n) { Rcpp::stop("Dimension mismatch for upper"); }

	double* x = new double[n];
	double* lo = new double[n];
	double* hi = new double[n];
	int* nbd = new int[n];

	for (unsigned int i = 0; i < n; i++) {
		x[i] = init(i);
		lo[i] = lower[i];
		hi[i] = upper[i];

		if (std::isinf(lower[i]) && std::isinf(upper[i])) {
			nbd[i] = 0;
		} else if (std::isinf(lower[i])) {
			nbd[i] = 3;
		} else if (std::isinf(upper[i])) {
			nbd[i] = 1;
		} else {
			nbd[i] = 2;
		}
	}

	// Make non-const copies of f and g
	dfv ff = f;
	vfv gg = g;

	lbfgsb_adapter adapter(ff, gg, args.fnscale);
	int fail;
	char msg[64];

	/*
	* *** L-BFGS-B ***
	* Call lbfgsb in R's C interface for Limited memory Broyden-Fletcher-
	* Goldfarb-Shanno algorithm.
	*
	* Arguments:
	*
	* 1. int n: number of parameters [In]
	* 2. int lmm maximum number of variable metric corrections [In]
	* 3. double *x: optimization variable [In/Out]
	* 4. double *lower: vector of lower bounds [In]
	* 5. double *upper: vector of upper bounds [In]
	* 6. int *nbd: type of bounds imposed on optimization variable [In]
	*	- 0 if x(i) is unbounded
	*	- 1 if x(i) has only a lower bound
	*	- 2 if x(i) has both lower and upper bounds
	*	- 3 if x(i) has only an upper bound
	* 7. double *Fmin: objective value at which optimum is found [Out]
	* 8. optimfn fn: objective function [In]
	* 9. optimgf gr: gradient function [In]
	* 10. int *fail: return code [Out]
	* 11. void *ex: external data to pass to the objective function [In]
	* 12. double factr: controls convergence. See `?optim` [In]
	* 13. double pgtol: controls convergence. See `?optim` [In]
	* 14. int *fncount: number of times the objective function was called [Out]
	* 15. int *grcount: number of times the gradient function was called [Out]
	* 16. int maxit: maximum number of iterations [In]
	* 17. char* msg: string with additional information from the optimizer [Out]
	* 18. int trace: print progress info if positive; six levels for lbfgs [In]
	* 19. int nREPORT: frequency of reports [In]
	*
	* Return: void
	*/

	lbfgsb(
		n,                     // 1
		args.lmm,              // 2
		x,                     // 3
		lo,                    // 4
		hi,                    // 5
		nbd,                   // 6
		&out.value,            // 7
		lbfgsb_adapter::eval,  // 8
		lbfgsb_adapter::grad,  // 9
		&fail,                 // 10
		&adapter,              // 11
		args.factr,            // 12
		args.pgtol,            // 13
		&out.fncount,          // 14
		&out.grcount,          // 15
		args.maxit,            // 16
		msg,                   // 17
		args.trace,            // 18
		args.report            // 19
	);

	out.message = msg;
	out.par.assign(x, x + n);
	out.status = lbfgsb_status(fail);
	out.value *= args.fnscale;

	delete[] x;
	delete[] lo;
	delete[] hi;
	delete[] nbd;

	return out;
}

inline lbfgsb_result lbfgsb(const Rcpp::NumericVector& init,
	const dfv& f, const lbfgsb_args& args)
{
	// Make g a call for numerical gradient
	const vfv& g = [&](const Rcpp::NumericVector& par) {
		const gradient_result& out = gradient(f, par, args.deriv_args);
		return Rcpp::NumericVector(out.value.begin(), out.value.end());
	};

	return lbfgsb(init, f, g, args);
}

inline lbfgsb_result lbfgsb(const Rcpp::NumericVector& init,
	const dfv& f, const vfv& g)
{
	lbfgsb_args args;
	return lbfgsb(init, f, g, args);
}


inline lbfgsb_result lbfgsb(const Rcpp::NumericVector& init,
	const dfv& f)
{
	lbfgsb_args args;
	return lbfgsb(init, f, args);
}

}

#endif
