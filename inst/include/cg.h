#ifndef FNTL_CG_H
#define FNTL_CG_H

#include <R_ext/Applic.h>
#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "args.h"
#include "gradient.h"
#include "result.h"

// This is for internal use only
class cg_adapter
{
private:
	const fntl::dfv& _f;
	const fntl::vfv& _g;
	double _fnscale;

public:
	cg_adapter(const fntl::dfv& f, const fntl::vfv& g, double fnscale)
		: _f(f), _g(g), _fnscale(fnscale)
	{
	}

	double get_fnscale() const { return _fnscale; }
	const fntl::dfv& get_f() const { return _f; }
	const fntl::vfv& get_g() const { return _g; }

	static double eval(int n, double* par, void* ex) {
		const cg_adapter* p = static_cast<const cg_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::dfv& f = p->get_f();
		return p->get_fnscale() * f(x);
	}

	static void grad(int n, double* par, double* gr, void* ex) {
		const cg_adapter* p = static_cast<const cg_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::vfv& g = p->get_g();
		const Rcpp::NumericVector& out = p->get_fnscale() * g(x);
		for (int i = 0; i < n; i++) {
			gr[i] = out(i);
		}
	}
};

namespace fntl {

inline cg_result cg(const Rcpp::NumericVector& init,
	const dfv& f, const vfv& g, const cg_args& args)
{
	cg_result out;
	unsigned int n = init.length();

	double* xin = new double[n];
	double* xout = new double[n];

	for (unsigned int i = 0; i < n; i++) {
		xin[i] = init(i);
	}

	// Make non-const copies of f and g
	dfv ff = f;
	vfv gg = g;

	cg_adapter adapter(ff, gg, args.fnscale);
	int fail;

	/*
	* *** CG ***
	* Call cgmin in R's C interface for the Conjugate Gradient algorithm.
	*
	* Arguments:
	* 1. int n: number of parameters [In]
	* 2. double* Bvec: initial value for optimization variable [In]
	* 3. double* X: final value for optimization variable [Out]
	* 4. double* Fmin: objective value at which optimum is found [Out]
	* 5. optimfn fminfn: objective function [In]
	* 6. optimgf fmingr: gradient function [In]
	* 7. int* fail: return code [Out]
	* 8. double abstol: absolute convergence tolerance [In]
	* 9. double intol:  user-initialized conversion tolerance [In]
	* 10. void* ex: external data to pass to the objective function [In]
	* 11. int type: type of update used in CG [In]
	* 12. int trace: print progress info if nonzero [In]
	* 13. int* fncount: number of times the objective function was called [Out]
	* 14. int* grcount: number of times the gradient function was called [Out]
	* 15. int maxit: maximum number of iterations [In]
	*
	* Return: void
	*/

	cgmin(
		n,                 // 1
		xin,               // 2
		xout,              // 3
		&out.value,        // 4
		cg_adapter::eval,  // 5
		cg_adapter::grad,  // 6
		&fail,             // 7
		args.abstol,       // 8
		args.reltol,       // 9
		&adapter,          // 10
		args.type,         // 11
		args.trace,        // 12
		&out.fncount,      // 13
		&out.grcount,      // 14
		args.maxit         // 15
	);

	out.par.assign(xout, xout + n);
	out.status = cg_status(fail);
	out.value *= args.fnscale;

	delete[] xin;
	delete[] xout;

	return out;
}

inline cg_result cg(const Rcpp::NumericVector& init,
	const dfv& f, const cg_args& args)
{
	// Make g a call for numerical gradient
	const vfv& g = [&](const Rcpp::NumericVector& par) {
		const gradient_result& out = gradient(f, par, args.deriv_args);
		return Rcpp::NumericVector(out.value.begin(), out.value.end());
	};

	return cg(init, f, g, args);
}

inline cg_result cg(const Rcpp::NumericVector& init, const dfv& f,
	const vfv& g)
{
	cg_args args;
	return cg(init, f, g, args);
}


inline cg_result cg(const Rcpp::NumericVector& init, const dfv& f)
{
	cg_args args;
	return cg(init, f, args);
}

}

#endif
