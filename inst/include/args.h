#ifndef FNTL_ARGS_H
#define FNTL_ARGS_H

/*
* This code follows a specific structure so that we can use the `as` and `wrap`
* constructs to serialize between structs and Rcpp Lists. The structs are
* defined first without Rcpp included yet, then Rcpp is included and the
* implementations for serialization operations are give.
*
* The pattern we follow here is referred to as "intrusive" (rather than
* "non-intrusive") because `wrap` and `as` are defined via member functions.
*
* See the following articles:
* <https://gallery.rcpp.org/articles/custom-templated-wrap-and-as-for-seamingless-interfaces>
* <https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-extending.pdf>
*
* The issue also came up in Stack\ Overflow threads such as:
* <https://stackoverflow.com/questions/51110244/in-rcpp-how-to-get-a-user-defined-structure-from-c-into-r>
* <https://stackoverflow.com/questions/74887786/specialising-rcppas-for-stdarray>
*/

#include <RcppCommon.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

namespace fntl {

struct richardson_args
{
	double delta = 0.5;
	unsigned int maxiter = 10;
	double h = 1;
	double tol = mach_eps_4r;
	double accuracy_factor = R_PosInf;

	richardson_args() { };
	richardson_args(SEXP obj);
	operator SEXP() const;
};

struct findroot_args
{
	double tol = mach_eps_4r;
	unsigned int maxiter = 1000;
	error_action action = error_action::STOP;
	unsigned int report_period = uint_max;

	findroot_args() { };
	findroot_args(SEXP obj);
	operator SEXP() const;
};

struct optimize_args
{
	double fnscale = 1;
	double tol = mach_eps_2r;
	unsigned int maxiter = 1000;
	unsigned int report_period = uint_max;
	error_action action = error_action::STOP;

	optimize_args() { };
	optimize_args(SEXP obj);
	operator SEXP() const;
};

struct integrate_args
{
	unsigned int subdivisions = 100L;
	double rel_tol = mach_eps_4r;
	double abs_tol = mach_eps_4r;
	bool stop_on_error = true;

	integrate_args() { };
	integrate_args(SEXP obj);
	operator SEXP() const;
};

struct cg_args
{
	double parscale = 1;  // TBD: use this or remove it
	double fnscale = 1;
	double abstol = R_NegInf;
	double reltol = mach_eps_2r;
	int type = 1;
	int trace = 0;
	int maxit = 100;
	richardson_args deriv_args;

	cg_args() { };
	cg_args(SEXP obj);
	operator SEXP() const;
};

struct bfgs_args
{
	double parscale = 1;  // TBD: use this or remove it
	int trace = 0;
	double fnscale = 1;
	int maxit = 100;
	int report = 10;
	double abstol = R_NegInf;
	double reltol = mach_eps_2r;
	richardson_args deriv_args;

	bfgs_args() { };
	bfgs_args(SEXP obj);
	operator SEXP() const;
};

struct lbfgsb_args
{
	std::vector<double> lower;
	std::vector<double> upper;
	double parscale = 1;  // TBD: use this or remove it
	int trace = 0;
	double fnscale = 1;
	int lmm = 5;
	int maxit = 100;
	int report = 10;
	double factr = 1e7;
	double pgtol = 0;
	richardson_args deriv_args;

	lbfgsb_args() { };
	lbfgsb_args(SEXP obj);
	operator SEXP() const;
};

struct neldermead_args
{
	double alpha = 1.0;
	double beta = 0.5;
	double gamma = 2.0;
	unsigned int trace = 0;
	double abstol = R_NegInf;
	double reltol = mach_eps_2r;
	unsigned int maxit = 500;
	double fnscale = 1.0;

	neldermead_args() { };
	neldermead_args(SEXP obj);
	operator SEXP() const;
};

struct nlm_args
{
	std::vector<double> typsize;
	unsigned int print_level = 0;
	double fscale = 1;
	double fnscale = 1;
	unsigned int ndigit = 12;
	double gradtol = 1e-6;
	double stepmax = R_PosInf;
	double steptol = 1e-6;
	int iterlim = 100;
	unsigned int method = 1;
	double trust_radius = 1.0;

	nlm_args() { };
	nlm_args(SEXP obj);
	operator SEXP() const;
};

}

#include <Rcpp.h>

namespace fntl {

/*
* Constructors from SEXP objects
*/

inline findroot_args::findroot_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "action", "tol", "maxiter",
		"report_period" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	if (x.containsElementNamed("action")) {
		unsigned int ac = x["action"];
		action = error_action(ac);
	}

	tol = x.containsElementNamed("tol") ? x["tol"] : tol;
	maxiter = x.containsElementNamed("maxiter") ? x["maxiter"] : maxiter;
	report_period = x.containsElementNamed("report_period") ? x["report_period"] : report_period;
}

inline optimize_args::optimize_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "action", "fnscale", "tol",
		"maxiter", "report_period" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	if (x.containsElementNamed("action")) {
		unsigned int ac = x["action"];
		action = error_action(ac);
	}

	fnscale = x.containsElementNamed("fnscale") ? x["fnscale"] : fnscale;
	tol = x.containsElementNamed("tol") ? x["tol"] : tol;
	maxiter = x.containsElementNamed("maxiter") ? x["maxiter"] : maxiter;
	report_period = x.containsElementNamed("report_period") ? x["report_period"] : report_period;
}

inline integrate_args::integrate_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "subdivisions", "rel_tol", "abs_tol",
		"stop_on_error" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	subdivisions = x.containsElementNamed("subdivisions") ? x["subdivisions"] : subdivisions;
	rel_tol = x.containsElementNamed("rel_tol") ? x["rel_tol"] : rel_tol;
	abs_tol = x.containsElementNamed("abs_tol") ? x["abs_tol"] : abs_tol;
	stop_on_error = x.containsElementNamed("stop_on_error") ? x["stop_on_error"] : stop_on_error;
}

inline cg_args::cg_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	if (x.containsElementNamed("deriv_args")) {
		deriv_args = x["deriv_args"];
	}

	const Rcpp::StringVector& ex_names = { "parscale", "fnscale", "abstol",
		"reltol", "type", "trace", "maxit", "deriv_args" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	parscale = x.containsElementNamed("parscale") ? x["parscale"] : parscale;
	fnscale = x.containsElementNamed("fnscale") ? x["fnscale"] : fnscale;
	abstol = x.containsElementNamed("abstol") ? x["abstol"] : abstol;
	reltol = x.containsElementNamed("reltol") ? x["reltol"] : reltol;
	type = x.containsElementNamed("type") ? x["type"] : type;
	trace = x.containsElementNamed("trace") ? x["trace"] : trace;
	maxit = x.containsElementNamed("maxit") ? x["maxit"] : maxit;
}

inline bfgs_args::bfgs_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	if (x.containsElementNamed("deriv_args")) {
		deriv_args = x["deriv_args"];
	}

	const Rcpp::StringVector& ex_names = { "parscale", "trace", "fnscale",
		"maxit", "report", "abstol", "reltol", "deriv_args" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	parscale = x.containsElementNamed("parscale") ? x["parscale"] : parscale;
	trace = x.containsElementNamed("trace") ? x["trace"] : trace;
	fnscale = x.containsElementNamed("fnscale") ? x["fnscale"] : fnscale;
	maxit = x.containsElementNamed("maxit") ? x["maxit"] : maxit;
	report = x.containsElementNamed("report") ? x["report"] : report;
	abstol = x.containsElementNamed("abstol") ? x["abstol"] : abstol;
	reltol = x.containsElementNamed("reltol") ? x["reltol"] : reltol;
}

inline lbfgsb_args::lbfgsb_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "lower", "upper", "deriv_args",
		"parscale", "trace", "fnscale", "lmm", "maxit", "report", "factr",
		"pgtol" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	if (x.containsElementNamed("lower")) {
		const Rcpp::NumericVector& lo = x["lower"];
		if (lo.size() > 0) {
			lower.assign(lo.begin(), lo.end());
		}
	}

	if (x.containsElementNamed("upper")) {
		const Rcpp::NumericVector& hi = x["upper"];
		if (hi.size() > 0) {
			upper.assign(hi.begin(), hi.end());
		}
	}

	if (x.containsElementNamed("deriv_args")) {
		deriv_args = x["deriv_args"];
	}

	parscale = x.containsElementNamed("parscale") ? x["parscale"] : parscale;
	trace = x.containsElementNamed("trace") ? x["trace"] : trace;
	fnscale = x.containsElementNamed("fnscale") ? x["fnscale"] : fnscale;
	lmm = x.containsElementNamed("lmm") ? x["lmm"] : lmm;
	maxit = x.containsElementNamed("maxit") ? x["maxit"] : maxit;
	report = x.containsElementNamed("report") ? x["report"] : report;
	factr = x.containsElementNamed("factr") ? x["factr"] : factr;
	pgtol = x.containsElementNamed("pgtol") ? x["pgtol"] : pgtol;
}

inline neldermead_args::neldermead_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "alpha", "beta", "gamma",
		"trace", "abstol", "reltol", "maxit", "fnscale", "deriv_args" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	alpha = x.containsElementNamed("alpha") ? x["alpha"] : alpha;
	beta = x.containsElementNamed("beta") ? x["beta"] : beta;
	gamma = x.containsElementNamed("gamma") ? x["gamma"] : gamma;
	trace = x.containsElementNamed("trace") ? x["trace"] : trace;
	abstol = x.containsElementNamed("abstol") ? x["abstol"] : abstol;
	reltol = x.containsElementNamed("reltol") ? x["reltol"] : reltol;
	maxit = x.containsElementNamed("maxit") ? x["maxit"] : maxit;
	fnscale = x.containsElementNamed("fnscale") ? x["fnscale"] : fnscale;
}

inline nlm_args::nlm_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "typsize", "print_level", "fscale",
		"fnscale", "ndigit", "gradtol", "stepmax", "steptol", "iterlim",
		"method", "trust_radius" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	typsize = x.containsElementNamed("typsize") ? x["typsize"] : typsize;
	print_level = x.containsElementNamed("print_level") ? x["print_level"] : print_level;
	fscale = x.containsElementNamed("fscale") ? x["fscale"] : fscale;
	fnscale = x.containsElementNamed("fnscale") ? x["fnscale"] : fnscale;
	ndigit = x.containsElementNamed("ndigit") ? x["ndigit"] : ndigit;
	gradtol = x.containsElementNamed("gradtol") ? x["gradtol"] : gradtol;
	stepmax = x.containsElementNamed("stepmax") ? x["stepmax"] : stepmax;
	steptol = x.containsElementNamed("steptol") ? x["steptol"] : steptol;
	iterlim = x.containsElementNamed("iterlim") ? x["iterlim"] : iterlim;
	method = x.containsElementNamed("method") ? x["method"] : method;
	trust_radius = x.containsElementNamed("trust_radius") ? x["trust_radius"] : trust_radius;
};

inline richardson_args::richardson_args(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "delta", "maxiter", "h", "tol",
		"accuracy_factor" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	delta = x.containsElementNamed("delta") ? x["delta"] : delta;
	maxiter = x.containsElementNamed("maxiter") ? x["maxiter"] : maxiter;
	h = x.containsElementNamed("h") ? x["h"] : h;
	tol = x.containsElementNamed("tol") ? x["tol"] : tol;
	accuracy_factor = x.containsElementNamed("accuracy_factor") ? x["accuracy_factor"] : accuracy_factor;
}

/*
* Conversation operators to SEXP objects
*/

inline findroot_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("tol") = tol,
		Rcpp::Named("maxiter") = maxiter,
		Rcpp::Named("action") = to_underlying(action),
		Rcpp::Named("report_period") = report_period
	);
}

inline optimize_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("fnscale") = fnscale,
		Rcpp::Named("tol") = tol,
		Rcpp::Named("maxiter") = maxiter,
		Rcpp::Named("report_period") = report_period,
		Rcpp::Named("action") = to_underlying(action)
	);
}

inline integrate_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("subdivisions") = subdivisions,
		Rcpp::Named("rel_tol") = rel_tol,
		Rcpp::Named("abs_tol") = abs_tol,
		Rcpp::Named("stop_on_error") = stop_on_error
	);
}

inline cg_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("deriv_args") = deriv_args,
		Rcpp::Named("parscale") = parscale,
		Rcpp::Named("fnscale") = fnscale,
		Rcpp::Named("abstol") = abstol,
		Rcpp::Named("reltol") = reltol,
		Rcpp::Named("type") = type,
		Rcpp::Named("trace") = trace,
		Rcpp::Named("maxit") = maxit
	);
}

inline bfgs_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("deriv_args") = deriv_args,
		Rcpp::Named("parscale") = parscale,
		Rcpp::Named("trace") = trace,
		Rcpp::Named("fnscale") = fnscale,
		Rcpp::Named("maxit") = maxit,
		Rcpp::Named("report") = report,
		Rcpp::Named("abstol") = abstol,
		Rcpp::Named("reltol") = reltol
	);
}

inline lbfgsb_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("lower") = lower,
		Rcpp::Named("upper") = upper,
		Rcpp::Named("deriv_args") = deriv_args,
		Rcpp::Named("parscale") = parscale,
		Rcpp::Named("trace") = trace,
		Rcpp::Named("fnscale") = fnscale,
		Rcpp::Named("lmm") = lmm,
		Rcpp::Named("maxit") = maxit,
		Rcpp::Named("report") = report,
		Rcpp::Named("factr") = factr,
		Rcpp::Named("pgtol") = pgtol
	);
}

inline neldermead_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("alpha") = alpha,
		Rcpp::Named("beta") = beta,
		Rcpp::Named("gamma") = gamma,
		Rcpp::Named("trace") = trace,
		Rcpp::Named("abstol") = abstol,
		Rcpp::Named("reltol") = reltol,
		Rcpp::Named("maxit") = maxit,
		Rcpp::Named("fnscale") = fnscale
	);
}

inline nlm_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("typsize") = typsize,
		Rcpp::Named("print_level") = print_level,
		Rcpp::Named("fscale") = fscale,
		Rcpp::Named("fnscale") = fnscale,
		Rcpp::Named("ndigit") = ndigit,
		Rcpp::Named("gradtol") = gradtol,
		Rcpp::Named("stepmax") = stepmax,
		Rcpp::Named("steptol") = steptol,
		Rcpp::Named("iterlim") = iterlim,
		Rcpp::Named("method") = method,
		Rcpp::Named("trust_radius") = trust_radius
	);
}

inline richardson_args::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("delta") = delta,
		Rcpp::Named("maxiter") = maxiter,
		Rcpp::Named("h") = h,
		Rcpp::Named("tol") = tol,
		Rcpp::Named("accuracy_factor") = accuracy_factor
	);
}

}

#endif
