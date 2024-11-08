#ifndef RCPP_INTERFACE_H
#define RCPP_INTERFACE_H

#include <Rcpp.h>

//' Numerical Derivatives via Finite Differences
//'
//' @param f Function to differentiate.
//' @param x Scalar at which to evaluate the derivative.
//' @param i First coordinate to differentiate.
//' @param j Second coordinate to differentiate.
//' @param h Step size in the first coordinate.
//' @param h_i Step size in the first coordinate.
//' @param h_j Step size in the second coordinate.
//' @param fd_type Type of derivative: `0` for symmetric difference, `1` for
//' forward difference, and `2` for backward difference.
//' @param args List of additional arguments from the function \code{richardson_args}.
//'
//' @examples
//' args = richardson_args()
//'
//' f = sin   # Try 2nd derivatives of a univariate function
//' x0 = 0.5
//' print(-sin(x0))  ## Exact answer for f''(x0)
//'
//' fd_deriv2(f, x0, i = 0, j = 0, h_i = 0.001, h_j = 0.001, fd_type = 0)
//' fd_deriv2(f, x0, i = 0, j = 0, h_i = 0.001, h_j = 0.001, fd_type = 1)
//' fd_deriv2(f, x0, i = 0, j = 0, h_i = 0.001, h_j = 0.001, fd_type = 2)
//'
//' deriv2(f, x0, i = 0, j = 0, args, fd_type = 0)
//'
//' # Try 2nd derivatives of a bivariate function
//' f = function(x) { sin(x[1]) + cos(x[2]) }
//' x0 = c(0.5, 0.25)
//'
//' print(-sin(x0[1]))  ## Exact answer for f_xx(x0)
//' print(-cos(x0[2]))  ## Exact answer for f_yy(x0)
//' print(0)         ## Exact answer for f_xy(x0,y0)
//'
//' numDeriv::hessian(f, x0)
//'
//' fd_deriv2(f, x0, i = 0, j = 0, h_i = 0.001, h_j = 0.001, fd_type = 0)
//' fd_deriv2(f, x0, i = 0, j = 0, h_i = 0.001, h_j = 0.001, fd_type = 1)
//' fd_deriv2(f, x0, i = 0, j = 0, h_i = 0.001, h_j = 0.001, fd_type = 2)
//'
//' fd_deriv2(f, x0, i = 0, j = 1, h_i = 0.001, h_j = 0.001, fd_type = 0)
//' fd_deriv2(f, x0, i = 0, j = 1, h_i = 0.001, h_j = 0.001, fd_type = 1)
//' fd_deriv2(f, x0, i = 0, j = 1, h_i = 0.001, h_j = 0.001, fd_type = 2)
//'
//' fd_deriv2(f, x0, i = 1, j = 1, h_i = 0.001, h_j = 0.001, fd_type = 0)
//' fd_deriv2(f, x0, i = 1, j = 1, h_i = 0.001, h_j = 0.001, fd_type = 1)
//' fd_deriv2(f, x0, i = 1, j = 1, h_i = 0.001, h_j = 0.001, fd_type = 2)
//'
//' deriv2(f, x0, i = 1, j = 1, args, fd_type = 0)
//' deriv2(f, x0, i = 1, j = 1, args, fd_type = 1)
//' deriv2(f, x0, i = 1, j = 1, args, fd_type = 2)
//'
//' @return
//' `fd_deriv1` and `fd_deriv2` return a single numeric value corresponding to
//' the first and second derivative via finite differences. `deriv1` and
//' `deriv2` return a list with the form of a `richardson_result` described in
//' section "Richardson Extrapolated Finite Differences" of the package
//' vignette.
//'
//' @name deriv
//'
//' @export
// [[Rcpp::export(name = "fd_deriv1")]]
double fd_deriv_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, double h, unsigned int fd_type);

//' @name deriv
//' @export
// [[Rcpp::export(name = "fd_deriv2")]]
double fd_deriv2_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, unsigned int j, double h_i, double h_j,
	unsigned int fd_type);

//' @name deriv
//' @export
// [[Rcpp::export(name = "deriv1")]]
Rcpp::List deriv_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, const Rcpp::List& args, unsigned int fd_type);

//' @name deriv
//' @export
// [[Rcpp::export(name = "deriv2")]]
Rcpp::List deriv2_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	unsigned int i, unsigned int j, const Rcpp::List& args,
	unsigned int fd_type);


//' Numerical Gradient Vector
//'
//' @param f Function to differentiate.
//' @param x Vector at which to evaluate the gradient.
//' @param args List of additional arguments from the function \code{richardson_args}.
//'
//' @examples
//' f = function(x) { sum(sin(x)) }
//' args = richardson_args()
//' x0 = seq(0, 1, length.out = 5)
//' cos(x0)  ## Exact answer
//' gradient0(f, x0, args)
//' numDeriv::grad(f, x0)
//'
//' @return
//' A list with the form of a `gradient_result` described in section "Gradient"
//' of the package vignette.
//'
//' @export
// [[Rcpp::export(name = "gradient0")]]
Rcpp::List gradient_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	const Rcpp::List& args);

//' Numerical Jacobian Matrix
//'
//' @param f Function to differentiate.
//' @param x Vector at which to evaluate the Jacobian.
//' @param args List of additional arguments from the function \code{richardson_args}.
//'
//' @examples
//' f = function(x) { cumsum(sin(x)) }
//' x0 = seq(1, 10, length.out = 5)
//' args = richardson_args()
//' out = jacobian0(f, x0, args)
//' print(out$value)
//' numDeriv::jacobian(f, x0)
//'
//' @return
//' A list with the form of a `jacobian_result` described in section "Jacobian"
//' of the package vignette.
//'
//' @export
// [[Rcpp::export(name = "jacobian0")]]
Rcpp::List jacobian_rcpp(const Rcpp::Function& f, const Rcpp::NumericVector& x,
	const Rcpp::List& args);

//' Numerical Hessian
//'
//' @param f Function to differentiate.
//' @param x Vector at which to evaluate the Hessian.
//' @param args List of additional arguments from the function \code{richardson_args}.
//'
//' @examples
//' f = function(x) { sum(x^2) }
//' x0 = seq(1, 10, length.out = 5)
//' args = richardson_args()
//' hessian0(f, x0, args)
//' numDeriv::hessian(f, x0)
//'
//' @return
//' A list with the form of a `hessian_result` described in section "Hessian"
//' of the package vignette.
//'
//' @export
// [[Rcpp::export(name = "hessian0")]]
Rcpp::List hessian_rcpp(const Rcpp::Function& f,
	const Rcpp::NumericVector& x, const Rcpp::List& args);

//' Find Root
//'
//' @param f Function for which a root is desired.
//' @param lower Lower limit of search interval. Must be finite.
//' @param upper Upper limit of search interval. Must be finite.
//' @param args List of additional arguments from the function \code{findroot_args}.
//'
//' @examples
//' f = function(x) { x^2 - 1 }
//' args = findroot_args()
//' findroot_bisect(f, 0, 10, args)
//' findroot_brent(f, 0, 10, args)
//'
//' @return
//' A list with the form of a `findroot_result` described in section
//' "Root-Finding" of the package vignette.
//'
//' @name findroot
//'
//' @export
// [[Rcpp::export(name = "findroot_bisect")]]
Rcpp::List findroot_bisect_rcpp(const Rcpp::Function& f, double lower, double upper,
	const Rcpp::List& args);

//' @name findroot
//' @export
// [[Rcpp::export(name = "findroot_brent")]]
Rcpp::List findroot_brent_rcpp(const Rcpp::Function& f, double lower, double upper,
	const Rcpp::List& args);

//' Univariate Optimization
//'
//' @param f Function to optimize.
//' @param lower Lower limit of search interval. Must be finite.
//' @param upper Upper limit of search interval. Must be finite.
//' @param args List of additional arguments from the function \code{optimize_args}.
//'
//' @examples
//' f = function(x) { x^2 - 1 }
//' args = optimize_args()
//' goldensection(f, 0, 10, args)
//' optimize_brent(f, 0, 10, args)
//'
//' @return
//' A list with the form of a `optimize_result` described in section
//' "Univariate Optimization" of the package vignette.
//'
//' @name univariate-optimization
//'
//' @export
// [[Rcpp::export(name = "goldensection")]]
Rcpp::List goldensection_rcpp(const Rcpp::Function& f, double lower,
	double upper, const Rcpp::List& args);

//' @name univariate-optimization
//' @export
// [[Rcpp::export(name = "optimize_brent")]]
Rcpp::List optimize_brent_rcpp(const Rcpp::Function& f, double lower,
	double upper, const Rcpp::List& args);


//' Integration
//'
//' Compute the integral \eqn{\int_a^b f(x) dx}.
//'
//' @param f Function to integrate.
//' @param lower Lower limit of integral.
//' @param upper Upper limit of integral.
//' @param args List of additional arguments from the function \code{integrate_args}.
//'
//' @examples
//' f = function(x) { exp(-x^2 / 2) }
//' args = integrate_args()
//' integrate0(f, 0, 10, args)
//'
//' @return
//' A list with the form of a `integrate_result` described in section
//' "Integration" of the package vignette.
//'
//' @export
// [[Rcpp::export(name = "integrate0")]]
Rcpp::List integrate_rcpp(const Rcpp::Function& f, double lower, double upper,
	const Rcpp::List& args);

//'  Multivariate Optimization
//'
//' @param init Initial value
//' @param f Function \eqn{f} to optimize
//' @param g Gradient function of \eqn{f}.
//' @param h Hessian function of \eqn{f}.
//' @param args List of additional arguments for optimization.
//'
//' @details
//' The argument `args` should be a list constructed from one of the following
//' functions:
//'
//' - `bfgs_args` for BFGS;
//' - `lbfgsb_args` for L-BFGS-B;
//' - `cg_args` for CG;
//' - `neldermead_args` for Nelder-Mead;
//' - `nlm_args` for the Newton-type algorithm used in `nlm`.
//'
//' When `g` or `h` are omitted, the gradient or Hessian will be respectively
//' be computed via finite differences.
//'
//' @examples
//' f = function(x) { sum(x^2) }
//' g = function(x) { 2*x }
//' h = function(x) { 2*diag(length(x)) }
//' x0 = c(1,1)
//'
//' args = cg_args()
//' cg1(x0, f, g, args)
//' cg2(x0, f, args)
//'
//' args = bfgs_args()
//' bfgs1(x0, f, g, args)
//' bfgs2(x0, f, args)
//'
//' args = lbfgsb_args()
//' lbfgsb1(x0, f, g, args)
//' lbfgsb2(x0, f, args)
//'
//' args = neldermead_args()
//' neldermead(x0, f, args)
//'
//' args = nlm_args()
//' nlm1(x0, f, g, h, args)
//' nlm2(x0, f, g, args)
//' nlm3(x0, f, args)
//'
//' @return
//' A list with results corresponding to the specified function. See the
//' package vignette for further details.
//'
//' - `cg1` and `cg2` return a `cg_result` which is documented in the section
//'   "Conjugate Gradient".
//' - `bfgs1` and `bfgs2` return a `bfgs_result` which is documented in the
//'   section "BFGS".
//' - `lbfgsb1` and `lbfgsb2` return a `lbfgsb_result` which is documented in
//'   the section "L-BFGS-B".
//' - `neldermead` returns a `neldermead_result` which is documented in
//'   the section "Nelder-Mead".
//' - `nlm1`, `nlm2`, and `nlm3` return a `nlm_result` which is documented in
//'   the section "Newton-Type Algorithm for Nonlinear Optimization".
//'
//' @name multivariate-optimization
//'
//' @export
// [[Rcpp::export(name = "cg1")]]
Rcpp::List cg1_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::Function& g, const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "cg2")]]
Rcpp::List cg2_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "bfgs1")]]
Rcpp::List bfgs1_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::Function& g, const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "bfgs2")]]
Rcpp::List bfgs2_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "lbfgsb1")]]
Rcpp::List lbfgsb1_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::Function& g, const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "lbfgsb2")]]
Rcpp::List lbfgsb2_rcpp(const Rcpp::NumericVector& init,
	const Rcpp::Function& f, const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "neldermead")]]
Rcpp::List neldermead_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "nlm1")]]
Rcpp::List nlm1_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::Function& g, const Rcpp::Function& h, const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "nlm2")]]
Rcpp::List nlm2_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::Function& g, const Rcpp::List& args);

//' @name multivariate-optimization
//' @export
// [[Rcpp::export(name = "nlm3")]]
Rcpp::List nlm3_rcpp(const Rcpp::NumericVector& init, const Rcpp::Function& f,
	const Rcpp::List& args);

//' Matrix Apply Functions
//'
//' @param X A matrix
//' @param f The function to apply.
//'
//' @details
//' The `mat_apply`, `row_apply`, and `col_apply` C++ functions are intended to
//' operate like the following calls in R, respectively.
//' ```
//' apply(x, c(1,2), f)
//' apply(x, 1, f)
//' apply(x, 2, f)
//' ```
//'
//' The R functions exposed here are specific to numeric-valued matrices, but
//' the underlying C++ functions are intended to work with any type of Rcpp
//' Matrix.
//'
//' @examples
//' X = matrix(1:12, nrow = 4, ncol = 3)
//' mat_apply(X, f = function(x) { x^(1/3) })
//' row_apply(X, f = function(x) { sum(x^2) })
//' col_apply(X, f = function(x) { sum(x^2) })
//'
//' @return
//' `mat_apply` returns a matrix. `row_apply` and `col_apply` return a vector.
//' See section "Apply" of the package vignette for details.
//'
//' @name matrix_apply
//'
//' @export
// [[Rcpp::export(name = "mat_apply")]]
Rcpp::NumericMatrix mat_apply_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f);

//' @name matrix_apply
//' @export
// [[Rcpp::export(name = "row_apply")]]
Rcpp::NumericVector row_apply_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f);

//' @name matrix_apply
//' @export
// [[Rcpp::export(name = "col_apply")]]
Rcpp::NumericVector col_apply_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f);


//' Matrix Which Function
//'
//' @param X A matrix
//' @param f A predicate to apply to each element of \eqn{X}.
//'
//' @details
//' The `which` C++ functions are intended to operate like the following call
//' in R.
//' ```
//' which(f(X), arr.ind = TRUE) - 1
//' ```
//'
//' The R functions exposed here are specific to numeric-valued matrices, but
//' the underlying C++ functions are intended to work with any type of Rcpp
//' Matrix.
//'
//' @examples
//' X = matrix(1:12 / 6, nrow = 4, ncol = 3)
//' f = function(x) { x < 1 }
//' which0(X, f)
//'
//' @return
//' A matrix with two columns. Each row contains a row and column index
//' corresponding to an element of \eqn{X} that matches the criteria of \eqn{f}.
//' See section "Which" of the package vignette for details.
//'
//' @export
// [[Rcpp::export(name = "which0")]]
Rcpp::IntegerMatrix which_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::Function& f);

//' Outer Matrix
//'
//' Compute "outer" matrices and matrix-vector products based on a function
//' that operators on pairs of rows. See details.
//'
//' @param X A numerical matrix.
//' @param Y A numerical matrix.
//' @param f Function \eqn{f(x, y)} that operates on a pair of rows. Depending
//' on the context, rows \eqn{x} and \eqn{y} are both rows of \eqn{X}, or
//' \eqn{x} is a row from \eqn{X} and \eqn{y} is a row from from \eqn{Y}.
//' @param a A scalar vector.
//'
//' @details
//' The `outer1` function computes the \eqn{n \times n} symmetric matrix
//'
//' \deqn{
//' \text{\texttt outer1}(X, f) =
//' \begin{bmatrix}
//' f(x_1, x_1) & \cdots & f(x_1, x_n) \cr
//' \vdots & \ddots & \vdots \cr
//' f(x_n, x_1) & \cdots & f(x_n, x_n) \cr
//' \end{bmatrix}
//' }
//'
//' and the `outer1_matvec` operation computes the \eqn{n}-dimensional vector
//'
//' \deqn{
//' \text{\texttt outer1\_matvec}(X, f, a) =
//' \begin{bmatrix}
//' f(x_1, x_1) & \cdots & f(x_1, x_n) \cr
//' \vdots & \ddots & \vdots \cr
//' f(x_n, x_1) & \cdots & f(x_n, x_n) \cr
//' \end{bmatrix}
//' \begin{bmatrix}
//' a_1 \cr
//' \vdots \cr
//' a_n \cr
//' \end{bmatrix}.
//' }
//'
//' The `outer2` operation computes the \eqn{m \times n} matrix
//'
//' \deqn{
//' \text{\texttt outer2}(X, Y, f) =
//' \begin{bmatrix}
//' f(x_1, y_1) & \cdots & f(x_1, y_n) \cr
//' \vdots & \ddots & \vdots \cr
//' f(x_m, y_1) & \cdots & f(x_m, y_n) \cr
//' \end{bmatrix}
//' }
//'
//' and the `outer2_matvec` operation computes the \eqn{m}-dimensional vector
//'
//' \deqn{
//' \text{\texttt outer2\_matvec}(X, Y, f, a) =
//' \begin{bmatrix}
//' f(x_1, y_1) & \cdots & f(x_1, y_n) \cr
//' \vdots & \ddots & \vdots \cr
//' f(x_m, y_1) & \cdots & f(x_m, y_n) \cr
//' \end{bmatrix}
//' \begin{bmatrix}
//' a_1 \cr
//' \vdots \cr
//' a_n \cr
//' \end{bmatrix}.
//' }
//'
//' @examples
//' set.seed(1234)
//' f = function(x,y) { sum( (x - y)^2 ) }
//' X = matrix(rnorm(12), 6, 2)
//' Y = matrix(rnorm(10), 5, 2)
//' outer1(X, f)
//' outer2(X, Y, f)
//'
//' a = rep(1, 6)
//' b = rep(1, 5)
//' outer1_matvec(X, f, a)
//' outer2_matvec(X, Y, f, b)
//'
//' @return
//' `outer1` and `outer2` return a matrix. `outer1_matvec` and `outer2_matvec`
//' return a vector. See section "Outer" of the package vignette for details.
//'
//' @name outer
//'
//' @export
// [[Rcpp::export(name = "outer1")]]
Rcpp::NumericMatrix outer1_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::Function& f);

//' @name outer
//' @export
// [[Rcpp::export(name = "outer2")]]
Rcpp::NumericMatrix outer2_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::NumericMatrix& Y, const Rcpp::Function& f);

//' @name outer
//' @export
// [[Rcpp::export(name = "outer1_matvec")]]
Rcpp::NumericVector outer1_matvec_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::Function& f, const Rcpp::NumericVector& a);

//' @name outer
//' @export
// [[Rcpp::export(name = "outer2_matvec")]]
Rcpp::NumericVector outer2_matvec_rcpp(const Rcpp::NumericMatrix& X,
	const Rcpp::NumericMatrix& Y, const Rcpp::Function& f,
	const Rcpp::NumericVector& a);

//' Iteratively Solve a Linear System with Conjugate Gradient
//'
//' Solve the system \eqn{l(x) = b} where \eqn{l(x)} is a matrix-free
//' representation of the linear operation \eqn{Ax}.
//'
//' @param l A linear transformation of \eqn{x}.
//' @param b A vector.
//' @param init Initial value of solution.
//' @param args List of additional arguments from `cg_args`.
//'
//' @examples
//' set.seed(1234)
//'
//' n = 8
//' idx_diag = cbind(1:n, 1:n)
//' idx_ldiag = cbind(2:n, 1:(n-1))
//' idx_udiag = cbind(1:(n-1), 2:n)
//' b = rep(1, n)
//'
//' ## Solution by explicit computation of solve(A, b)
//' A = matrix(0, n, n)
//' A[idx_diag] = 2
//' A[idx_ldiag] = 1
//' A[idx_udiag] = 1
//' solve(A, b)
//'
//' ## Solve iteratively with solve_cg
//' f = function(x) { A %*% x }
//' args = cg_args()
//' init = rep(0, n)
//' solve_cg(f, b, init, args)
//'
//' @return
//' A list with the form of a `solve_cg_result` described in section "Conjugate
//' Gradient" of the package vignette.
//'
//' @export
// [[Rcpp::export(name = "solve_cg")]]
Rcpp::List solve_cg_rcpp(const Rcpp::Function& l,
	const Rcpp::NumericVector& b, const Rcpp::NumericVector& init,
	const Rcpp::List& args);

//' Functions for Truncated Distributions
//'
//' Density, CDF, quantile, and drawing functions for a univariate distribution
//' with density \eqn{f}, cdf, \eqn{F}, and quantile function \eqn{F^{-}}
//' truncated to the interval \eqn{[a ,b]}.
//'
//' @param n Number of draws.
//' @param x Vector of quantiles.
//' @param p Vector of probabilities.
//' @param lo Vector of lower limits.
//' @param hi Vector of upper limits.
//' @param f Density function with form `f(x, log)`.
//' @param F CDF function with signature `F(x, lower, log)`, where `x` is
//' numeric and `lower` and `log` are logical.
//' @param Finv Quantile function with signature `Finv(x, lower, log)`, where
//' `x` is numeric and `lower` and `log` are logical.
//' @param lower logical; if `TRUE`, probabilities are \eqn{P(X \leq x)};
//' otherwise, \eqn{P(X > x)}.
//' @param log logical; if `TRUE`, probabilities are given on log-scale.
//'
//' @return Vector with results.
//' `d_trunc` computes the density, `r_trunc` generates random deviates,
//' `p_trunc` computes the CDF, and `q_trunc` computes quantiles.
//'
//' @examples
//' library(tidyverse)
//'
//' m = 100  ## Length of sequence for density, CDF, etc
//' shape1 = 5
//' shape2 = 2
//' lo = 0.5
//' hi = 0.7
//'
//' # Density, CDF, and quantile function for untruncated distribution
//' ff = function(x, log) { dbeta(x, shape1, shape2, log = log) }
//' FF = function(x, lower, log) { pbeta(x, shape1, shape2, lower.tail = lower, log = log) }
//' FFinv = function(x, lower, log) { qbeta(x, shape1, shape2, lower.tail = lower, log = log) }
//'
//' # Compare truncated and untruncated densities
//' xseq = seq(0, 1, length.out = m)
//' lo_vec = rep(lo, m)
//' hi_vec = rep(hi, m)
//' f0seq = ff(xseq, log = FALSE)
//' fseq = d_trunc(xseq, lo_vec, hi_vec, ff, FF)
//' data.frame(x = xseq, f = fseq, f0 = f0seq) %>%
//'     ggplot() +
//'	    geom_line(aes(xseq, fseq)) +
//'	    geom_line(aes(xseq, f0seq), lty = 2) +
//'	    xlab("x") +
//'	    ylab("Density") +
//'     theme_minimal()
//'
//' # Compare truncated densities and empirical density of generated draws
//' n = 100000
//' lo_vec = rep(lo, n)
//' hi_vec = rep(hi, n)
//' x = r_trunc(n = n, lo_vec, hi_vec, FF, FFinv)
//' hist(x, probability = TRUE, breaks = 15)
//' points(xseq, fseq)
//'
//' # Compare empirical CDF of draws with CDF function
//' Femp = ecdf(x)
//' lo_vec = rep(lo, m)
//' hi_vec = rep(hi, m)
//' Fseq = p_trunc(xseq, lo_vec, hi_vec, FF)
//' data.frame(x = xseq, FF = Fseq) %>%
//'     mutate(F0 = Femp(x)) %>%
//'     ggplot() +
//'     geom_line(aes(xseq, FF), lwd = 1.2) +
//'     geom_line(aes(xseq, F0), col = "orange") +
//'     xlab("x") +
//'     ylab("Probability") +
//'     theme_minimal()
//'
//' # Compare empirical quantiles of draws with quantile function
//' pseq = seq(0, 1, length.out = m)
//' lo_vec = rep(lo, m)
//' hi_vec = rep(hi, m)
//' Finvseq = q_trunc(pseq, lo_vec, hi_vec, FF, FFinv)
//' Finvemp = quantile(x, prob = pseq)
//' data.frame(p = pseq, Finv = Finvseq, Finvemp = Finvemp) %>%
//'	    ggplot() +
//'	    geom_line(aes(pseq, Finv), lwd = 1.2) +
//'	    geom_line(aes(pseq, Finvemp), col = "orange") +
//'	    xlab("p") +
//'	    ylab("Quantile") +
//'	    theme_minimal()
//'
//' @name trunc
//' @export
// [[Rcpp::export(name = "d_trunc")]]
Rcpp::NumericVector d_trunc_rcpp(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const Rcpp::Function& f, const Rcpp::Function& F, bool log = false);

//' @name trunc
//' @export
// [[Rcpp::export(name = "p_trunc")]]
Rcpp::NumericVector p_trunc_rcpp(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const Rcpp::Function& F, bool lower = true, bool log = false);

//' @name trunc
//' @export
// [[Rcpp::export(name = "q_trunc")]]
Rcpp::NumericVector q_trunc_rcpp(const Rcpp::NumericVector& p,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const Rcpp::Function& F, const Rcpp::Function& Finv, bool lower = true,
	bool log = false);

//' @name trunc
//' @export
// [[Rcpp::export(name = "r_trunc")]]
Rcpp::NumericVector r_trunc_rcpp(unsigned int n, const Rcpp::NumericVector& lo,
	const Rcpp::NumericVector& hi, const Rcpp::Function& F,
	const Rcpp::Function& Finv);

//' Arguments
//'
//' Get an arguments list for internal methods with the default settings. This
//' object can be adjusted and passed to the respective function.
//'
//' @return
//' An argument list corresponding to the specified function. The elements of
//' the list are named and supplied with default values. See the package
//' vignette for further details.
//'
//' - `findroot_args` is documented in the section "Root-Finding".
//' - `optimize_args` is documented in the section "Univariate Optimization".
//' - `integrate_args` is documented in the section "Integration".
//' - `cg_args` is documented in the section "Conjugate Gradient".
//' - `bfgs_args` is documented in the section "BFGS".
//' - `lbfgsb_args` is documented in the section "L-BFGS-B".
//' - `neldermead_args`is documented in the section "Nelder-Mead".
//' - `nlm_args` is documented in the section "Newton-Type Algorithm for
//'   Nonlinear Optimization".
//' - `richardson_args` is documented in the section "Richardson
//'   Extrapolated Finite Differences".
//'
//' @name args
//' @export
// [[Rcpp::export(name = "findroot_args")]]
Rcpp::List findroot_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "optimize_args")]]
Rcpp::List optimize_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "integrate_args")]]
Rcpp::List integrate_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "cg_args")]]
Rcpp::List cg_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "bfgs_args")]]
Rcpp::List bfgs_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "lbfgsb_args")]]
Rcpp::List lbfgsb_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "neldermead_args")]]
Rcpp::List neldermead_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "nlm_args")]]
Rcpp::List nlm_args_rcpp();

//' @name args
//' @export
// [[Rcpp::export(name = "richardson_args")]]
Rcpp::List richardson_args_rcpp();

#endif
