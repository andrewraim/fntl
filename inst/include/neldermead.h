#ifndef FNTL_NELDERMEAD_H
#define FNTL_NELDERMEAD_H

#include <R.h>
#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "args.h"
#include "result.h"

// This is for internal use only
class neldermead_adapter
{
private:
	const fntl::dfv& _f;
	double _fnscale;

public:
	neldermead_adapter(const fntl::dfv& f, double fnscale)
	: _f(f), _fnscale(fnscale)
	{
	}

	double get_fnscale() const { return _fnscale; }
	const fntl::dfv& get_f() const { return _f; }

	static double eval(int n, double* par, void* ex) {
		const neldermead_adapter* p = static_cast<const neldermead_adapter*>(ex);
		Rcpp::NumericVector x(par, par + n);
		const fntl::dfv& f = p->get_f();
		return p->get_fnscale() * f(x);
	}
};

namespace fntl {


/*
* Nelder-Mead algorithm from R.
* See <https://cran.r-project.org/doc/manuals/R-exts.html#Optimization>
* and <https://stackoverflow.com/questions/12765304/calling-r-function-optim-from-c>
*/
inline neldermead_result neldermead(const Rcpp::NumericVector& init,
	const dfv& f, const neldermead_args& args)
{
	neldermead_result out;
	unsigned int n = init.length();

	double* xin = new double[n];
	double* xout = new double[n];

	for (unsigned int i = 0; i < n; i++) {
		xin[i] = init(i);
	}

	// Make a non-const copy
	dfv ff = f;

	neldermead_adapter adapter(ff, args.fnscale);
	int fail;

	/*
	* *** Nelder-Mead ***
	* Call nmmin in R's C interface.
	*
	* Arguments:
	*
	* 1. int n: number of parameters [In]
	* 2. double *xin: initial value [In]
	* 3. double *x: point at which optimum is found [Out]
	* 4. double *Fmin: objective value at which optimum is found [Out]
	* 5. optimfn fn: objective function [In]
	* 6. int *fail: true if the function failed [Out]
	* 7. double abstol: absolute tolerance [In]
	* 8. double intol: user-initialized conversion tolerance [In]
	* 9. void *ex: external data to pass to the objective function [In]
	* 10. double alpha: reflection factor [In]
	* 11. double beta: contraction and reduction factor [In]
	* 12. double gamma: extension factor [In]
	* 13. int trace: if positive, print progress info [In]
	* 14. int *fncount: number of times the objective function was called [Out]
	* 15. int maxit: maximum number of iterations [In]
	*
	* Return: void
	*/
	nmmin(
		n,                        // 1
		xin,                      // 2
		xout,                     // 3
		&out.value,               // 4
		neldermead_adapter::eval, // 5
		&fail,                    // 6
		R_NegInf,                 // 7
		args.reltol,              // 8
		&adapter,                 // 9
		args.alpha,               // 10
		args.beta,                // 11
		args.gamma,               // 12
		args.trace,               // 13
		&out.fncount,             // 14
		args.maxit                // 15
	);

	out.par.assign(xout, xout + n);
	out.status = neldermead_status(fail);
	out.value *= args.fnscale;

	delete[] xin;
	delete[] xout;

	return out;
}

inline neldermead_result neldermead(const Rcpp::NumericVector& init,
	const dfv& f)
{
	neldermead_args args;
	return neldermead(init, f, args);
}

}

#endif

