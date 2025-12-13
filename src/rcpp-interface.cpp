#include "rcpp-interface.h"
#include "fntl.h"

Rcpp::List findroot_args_rcpp()
{
	fntl::findroot_args args;
	return Rcpp::wrap(args);
}

Rcpp::List optimize_args_rcpp()
{
	fntl::optimize_args args;
	return Rcpp::wrap(args);
}

Rcpp::List integrate_args_rcpp()
{
	fntl::integrate_args args;
	return Rcpp::wrap(args);
}

Rcpp::List cg_args_rcpp()
{
	fntl::cg_args args;
	return Rcpp::wrap(args);
}

Rcpp::List bfgs_args_rcpp()
{
	fntl::bfgs_args args;
	return Rcpp::wrap(args);
}

Rcpp::List lbfgsb_args_rcpp()
{
	fntl::lbfgsb_args args;
	return Rcpp::wrap(args);
}

Rcpp::List neldermead_args_rcpp()
{
	fntl::neldermead_args args;
	return Rcpp::wrap(args);
}

Rcpp::List nlm_args_rcpp()
{
	fntl::nlm_args args;
	return Rcpp::wrap(args);
}

Rcpp::List richardson_args_rcpp()
{
	fntl::richardson_args args;
	return Rcpp::wrap(args);
}

Rcpp::List gradient_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::richardson_args>(args);
	const auto& out = fntl::gradient(ff, x, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List hessian_rcpp(const Rcpp::Function& f,
	const Rcpp::NumericVector& x, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::richardson_args>(args);
	const auto& out = fntl::hessian(ff, x, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List jacobian_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	const Rcpp::List& args)
{
	const fntl::vfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx;
	};

	const auto& cargs = Rcpp::as<fntl::richardson_args>(args);
	const auto& out = fntl::jacobian(ff, x, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List findroot_bisect_rcpp(const Rcpp::Function& f, double lower,
	double upper, const Rcpp::List& args)
{
	const fntl::dfd& ff = [&](double x) {
		const Rcpp::NumericVector& xx = Rcpp::NumericVector::create(x);
		const Rcpp::NumericVector& fx = f(xx);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::findroot_args>(args);
	const auto& out = fntl::findroot_bisect(ff, lower, upper, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List findroot_brent_rcpp(const Rcpp::Function& f, double lower, double upper,
	const Rcpp::List& args)
{
	const fntl::dfd& ff = [&](double x) {
		const Rcpp::NumericVector& xx = Rcpp::NumericVector::create(x);
		const Rcpp::NumericVector& fx = f(xx);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::findroot_args>(args);
	const auto& out = fntl::findroot_brent(ff, lower, upper, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List goldensection_rcpp(const Rcpp::Function& f, double lower,
	double upper, const Rcpp::List& args)
{
	const fntl::dfd& ff = [&](double x) {
		const Rcpp::NumericVector& xx = Rcpp::NumericVector::create(x);
		const Rcpp::NumericVector& fx = f(xx);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::optimize_args>(args);
	const auto& out =  goldensection(ff, lower, upper, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List optimize_brent_rcpp(const Rcpp::Function& f, double lower,
	double upper, const Rcpp::List& args)
{
	const fntl::dfd& ff = [&](double x) {
		const Rcpp::NumericVector& xx = Rcpp::NumericVector::create(x);
		const Rcpp::NumericVector& fx = f(xx);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::optimize_args>(args);
	const auto& out = optimize_brent(ff, lower, upper, cargs);
	return Rcpp::wrap(out);
}


Rcpp::List integrate_rcpp(const Rcpp::Function& f, double lower, double upper,
	const Rcpp::List& args)
{
	const fntl::dfd& ff = [&](double x) {
		const Rcpp::NumericVector& xx = Rcpp::NumericVector::create(x);
		const Rcpp::NumericVector& fx = f(xx);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::integrate_args>(args);
	const auto& out = integrate(ff, lower, upper, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List cg1_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::Function& g, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const fntl::vfv& gg = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& gx = g(x);
		return gx;
	};

	const auto& cargs = Rcpp::as<fntl::cg_args>(args);
	const auto& out = fntl::cg(init, ff, gg, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List cg2_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::cg_args>(args);
	const auto& out = fntl::cg(init, ff, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List bfgs1_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::Function& g, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const fntl::vfv& gg = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& gx = g(x);
		return gx;
	};

	const auto& cargs = Rcpp::as<fntl::bfgs_args>(args);
	const auto& out = fntl::bfgs(init, ff, gg, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List bfgs2_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::bfgs_args>(args);
	const auto& out = fntl::bfgs(init, ff, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List lbfgsb1_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::Function& g, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const fntl::vfv& gg = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& gx = g(x);
		return gx;
	};

	const auto& cargs = Rcpp::as<fntl::lbfgsb_args>(args);
	const auto& out = fntl::lbfgsb(init, ff, gg, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List lbfgsb2_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::lbfgsb_args>(args);
	const auto& out = fntl::lbfgsb(init, ff, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List neldermead_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::neldermead_args>(args);
	const auto& out = fntl::neldermead(init, ff, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List nlm1_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::Function& g, const Rcpp::Function& h, const Rcpp::List& args)
{
	const fntl::dfv& ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const fntl::vfv& gg = [&](const Rcpp::NumericVector& x) {
		return g(x);
	};

	const fntl::mfv& hh = [&](const Rcpp::NumericVector& x) {
		return h(x);
	};


	const auto& cargs = Rcpp::as<fntl::nlm_args>(args);
	const auto& out = fntl::nlm(init, ff, gg, hh, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List nlm2_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::Function& g, const Rcpp::List& args)
{
	const fntl::dfv ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const fntl::vfv gg = [&](const Rcpp::NumericVector& x) {
		return g(x);
	};


	const auto& cargs = Rcpp::as<fntl::nlm_args>(args);
	const auto& out = fntl::nlm(init, ff, gg, cargs);
	return Rcpp::wrap(out);
}

Rcpp::List nlm3_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::List& args)
{
	const fntl::dfv ff = [&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::nlm_args>(args);
	const auto& out = fntl::nlm(init, ff, cargs);
	return Rcpp::wrap(out);
}

Rcpp::NumericMatrix mat_apply_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f)
{
	const std::function<double(double)>& ff = [&](double x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	return fntl::mat_apply(X, ff);
}

Rcpp::NumericVector row_apply_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f)
{
	const std::function<double(const Rcpp::NumericVector&)>& ff =
	[&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	return fntl::row_apply(X, ff);
}

Rcpp::NumericVector col_apply_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f)
{
	const std::function<double(const Rcpp::NumericVector&)>& ff =
	[&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	return fntl::col_apply(X, ff);
}

Rcpp::IntegerMatrix which_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f)
{
	const std::function<bool(double)>& ff = [&](double x) {
		Rcpp::NumericVector xx = { x };
		const Rcpp::NumericVector& fx = f(xx);
		return fx(0);
	};

	return fntl::which(X, ff);
}


Rcpp::NumericMatrix outer1_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::Function& f)
{
	const std::function<double(const Rcpp::NumericVector&, const Rcpp::NumericVector&)>& ff =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& fxy = f(x, y);
		return fxy(0);
	};

	return fntl::outer(X, ff);
}

Rcpp::NumericMatrix outer2_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::NumericMatrix& Y, const Rcpp::Function& f)
{
	const std::function<double(const Rcpp::NumericVector&, const Rcpp::NumericVector&)>& ff =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& fxy = f(x, y);
		return fxy(0);
	};

	return fntl::outer(X, Y, ff);
}

Rcpp::NumericVector outer1_matvec_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::Function& f, const Rcpp::NumericVector& a)
{
	const std::function<double(const Rcpp::NumericVector&, const Rcpp::NumericVector&)>& ff =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& fxy = f(x, y);
		return fxy(0);
	};

	return fntl::outer_matvec(X, ff, a);
}

Rcpp::NumericVector outer2_matvec_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::NumericMatrix& Y, const Rcpp::Function& f,
	const Rcpp::NumericVector& a)
{
	const std::function<double(const Rcpp::NumericVector&, const Rcpp::NumericVector&)>& ff =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& fxy = f(x, y);
		return fxy(0);
	};

	return fntl::outer_matvec(X, Y, ff, a);
}

Rcpp::List outer1_triplet_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::Function& f, const Rcpp::Function& g)
{
	const fntl::dfvv& ff =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& fxy = f(x, y);
		return fxy(0);
	};

	const fntl::bfvv& gg =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& gxy = g(x, y);
		return gxy(0);
	};

	const auto& out = fntl::outer_triplet(X, ff, gg);
	return Rcpp::wrap(out);
}

Rcpp::List outer2_triplet_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::NumericMatrix& Y, const Rcpp::Function& f,
	const Rcpp::Function& g)
{
	const fntl::dfvv& ff =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& fxy = f(x, y);
		return fxy(0);
	};

	const fntl::bfvv& gg =
	[&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
		const Rcpp::NumericVector& gxy = g(x, y);
		return gxy(0);
	};

	const auto& out = fntl::outer_triplet(X, Y, ff, gg);
	return Rcpp::wrap(out);
}

Rcpp::List solve_cg_rcpp(const Rcpp::Function& l,
	const Rcpp::NumericVector& b, const Rcpp::NumericVector& init,
	const Rcpp::List& args)
{
	const std::function<Rcpp::NumericVector(const Rcpp::NumericVector&)>& ff =
	[&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = l(x);
		return fx;
	};

	const auto& cargs = Rcpp::as<fntl::cg_args>(args);
	const auto& out = fntl::solve_cg(ff, b, init, cargs);
	return Rcpp::wrap(out);
}

double fd_deriv_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, double h, unsigned int fd_type)
{
	const fntl::dfv& ff =
	[&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	fntl::fd_types ctype = fntl::fd_types(fd_type);
	return fntl::fd_deriv(ff, x, i, h, ctype);
}

double fd_deriv2_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, unsigned int j, double h_i, double h_j,
	unsigned int fd_type)
{
	const fntl::dfv& ff =
	[&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	fntl::fd_types ctype = fntl::fd_types(fd_type);
	return fntl::fd_deriv2(ff, x, i, j, h_i, h_j, ctype);
}

Rcpp::List deriv_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, const Rcpp::List& args, unsigned int fd_type)
{
	const fntl::dfv& ff =
	[&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::richardson_args>(args);
	fntl::fd_types ctype = fntl::fd_types(fd_type);
	const auto& out = fntl::deriv(ff, x, i, cargs, ctype);
	return Rcpp::wrap(out);
}

Rcpp::List deriv2_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, unsigned int j, const Rcpp::List& args,
	unsigned int fd_type)
{
	const fntl::dfv& ff =
	[&](const Rcpp::NumericVector& x) {
		const Rcpp::NumericVector& fx = f(x);
		return fx(0);
	};

	const auto& cargs = Rcpp::as<fntl::richardson_args>(args);
	fntl::fd_types ctype = fntl::fd_types(fd_type);
	const auto& out = fntl::deriv2(ff, x, i, j, cargs, ctype);
	return Rcpp::wrap(out);
}

Rcpp::NumericVector d_trunc_rcpp(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const Rcpp::Function& f, const Rcpp::Function& F, bool log)
{
	const fntl::density& ff = [&](double x, bool log) {
		const Rcpp::NumericVector& fx = f(x, log);
		return fx(0);
	};

	const fntl::cdf& FF = [&](double x, bool lower, bool log) {
		const Rcpp::NumericVector& Fx = F(x, lower, log);
		return Fx(0);
	};

	return fntl::d_trunc(x, lo, hi, ff, FF, log);
}

Rcpp::NumericVector p_trunc_rcpp(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const Rcpp::Function& F, bool lower, bool log)
{
	const fntl::cdf& FF = [&](double x, bool lower, bool log) {
		const Rcpp::NumericVector& Fx = F(x, lower, log);
		return Fx(0);
	};

	return fntl::p_trunc(x, lo, hi, FF, lower, log);
}

Rcpp::NumericVector q_trunc_rcpp(const Rcpp::NumericVector& p,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const Rcpp::Function& F, const Rcpp::Function& Finv, bool lower, bool log)
{
	const fntl::cdf& FF = [&](double x, bool lower, bool log) {
		const Rcpp::NumericVector& Fx = F(x, lower, log);
		return Fx(0);
	};

	const fntl::quantile& FFinv = [&](double x, bool lower, bool log) {
		const Rcpp::NumericVector& Finvx = Finv(x, lower, log);
		return Finvx(0);
	};

	return fntl::q_trunc(p, lo, hi, FF, FFinv, lower, log);
}

Rcpp::NumericVector r_trunc_rcpp(unsigned int n, const Rcpp::NumericVector& lo,
	const Rcpp::NumericVector& hi, const Rcpp::Function& F,
	const Rcpp::Function& Finv)
{
	const fntl::cdf& FF = [&](double x, bool lower, bool log) {
		const Rcpp::NumericVector& Fx = F(x, lower, log);
		return Fx(0);
	};

	const fntl::quantile& FFinv = [&](double x, bool lower, bool log) {
		const Rcpp::NumericVector& Finvx = Finv(x, lower, log);
		return Finvx(0);
	};

	return fntl::r_trunc(n, lo, hi, FF, FFinv);
}
