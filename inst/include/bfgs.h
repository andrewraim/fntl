#ifndef FNTL_BFGS_H
#define FNTL_BFGS_H

#include <R_ext/Applic.h>
#include <R.h>
#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "args.h"
#include "gradient.h"
#include "result.h"

// This is for internal use only
class bfgs_adapter
{
private:
	const fntl::dfv& _f;
	const fntl::vfv& _g;
	double _fnscale;

public:
	bfgs_adapter(const fntl::dfv& f,
		const fntl::vfv& g,
		double fnscale)
		: _f(f), _g(g), _fnscale(fnscale)
	{
	}

	double get_fnscale() const { return _fnscale; }
	const fntl::dfv& get_f() const { return _f; }
	const fntl::vfv& get_g() const { return _g; }

	static double eval(int n, double* par, void* ex) {
		const bfgs_adapter* p = static_cast<const bfgs_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::dfv& f = p->get_f();
		return p->get_fnscale() * f(x);
	}

	static void grad(int n, double* par, double* gr, void* ex) {
		const bfgs_adapter* p = static_cast<const bfgs_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::vfv& g = p->get_g();
		const Rcpp::NumericVector& out = p->get_fnscale() * g(x);
		for (int i = 0; i < n; i++) {
			gr[i] = out(i);
		}
	}
};

namespace fntl {

inline bfgs_result bfgs(const Rcpp::NumericVector& init,
	const dfv& f, const vfv& g, const bfgs_args& args)
{
	bfgs_result out;
	unsigned int n = init.length();

	double* x = new double[n];
	int* mask = new int[n];

	for (unsigned int i = 0; i < n; i++) {
		x[i] = init(i);
		mask[i] = 1;
	}

	// Make non-const copies of f and g
	dfv ff = f;
	vfv gg = g;

	bfgs_adapter adapter(ff, gg, args.fnscale);
	int fail;

	/*
	* *** BFGS ***
	* Call vmmin in R's C interface for the Broyden-Fletcher-Goldfarb-Shanno
	* algorithm.
	*
	* Arguments:
	* 1. int n0: number of parameters [In]
	* 2. double* b: optimization variable [In/Out]
	* 3. double* Fmin: objective value at which optimum is found [Out]
	* 4. optimfn fn: objective function [In]
	* 5. optimgf gr: gradient function [In]
	* 6. int maxit: maximum number of iterations [In]
	* 7. int trace: print progress info if positive; six levels for bfgs [In]
	* 8. int* mask: R takes this to be a vector of n ones [In]
	* 9. double abstol: absolute convergence tolerance [In]
	* 10. double reltol: relative convergence tolerance [In]
	* 11. int nREPORT: frequency of reports [In]
	* 12. void* ex: external data to pass to the objective function [In]
	* 13. int* fncount: number of times the objective function was called [Out]
	* 14. int* grcount: number of times the gradient function was called [Out]
	* 15. int* fail: return code [Out]
	*
	* Return: void
	*/

	vmmin(
		n,                   // 1
		x,                   // 2
		&out.value,          // 3
		bfgs_adapter::eval,  // 4
		bfgs_adapter::grad,  // 5
		args.maxit,          // 6
		args.trace,          // 7
		mask,                // 8
		args.abstol,         // 9
		args.reltol,         // 10
		args.report,         // 11
		&adapter,            // 12
		&out.fncount,        // 13
		&out.grcount,        // 14
		&fail                // 15
	);

	out.par.assign(x, x + n);
	out.status = bfgs_status(fail);
	out.value *= args.fnscale;

	delete[] x;
	delete[] mask;

	return out;
}

inline bfgs_result bfgs(const Rcpp::NumericVector& init,
	const dfv& f, const bfgs_args& args)
{
	// Make g a call for numerical gradient
	const vfv& g = [&](const Rcpp::NumericVector& par) {
		const gradient_result& out = gradient(f, par, args.deriv_args);
		return Rcpp::NumericVector(out.value.begin(), out.value.end());
	};

	return bfgs(init, f, g, args);
}

inline bfgs_result bfgs(const Rcpp::NumericVector& init, const dfv& f,
	const vfv& g)
{
	bfgs_args args;
	return bfgs(init, f, g, args);
}


inline bfgs_result bfgs(const Rcpp::NumericVector& init, const dfv& f)
{
	bfgs_args args;
	return bfgs(init, f, args);
}

}

#endif

