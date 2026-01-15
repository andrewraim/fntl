#ifndef FNTL_OUTER_H
#define FNTL_OUTER_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

namespace fntl {

/*
* Versions of outer that are specific to Rcpp matrices
*/

template <typename T, int RTYPE>
inline Rcpp::Matrix<RTYPE> outer(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(
		const Rcpp::Vector<RTYPE>&,
		const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int n = X.nrow();
	Rcpp::Matrix<RTYPE> out(n, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < j; i++) {
			out(i,j) = f(X.row(i), X.row(j));
			out(j,i) = out(i,j);
		}
	}

	for (unsigned int i = 0; i < n; i++) {
		out(i,i) = f(X.row(i), X.row(i));
	}

	return out;
}

template <typename T, int RTYPE>
inline Rcpp::Matrix<RTYPE> outer(
	const Rcpp::Matrix<RTYPE>& X,
	const Rcpp::Matrix<RTYPE>& Y,
	const std::function<T(
		const Rcpp::Vector<RTYPE>&,
		const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int m = X.nrow();
	unsigned int n = Y.nrow();
	Rcpp::Matrix<RTYPE> out(m, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			out(i,j) = f(X.row(i), Y.row(j));
		}
	}

	return out;
}

template <typename T, int RTYPE>
inline csc_mat<T> outer_sp(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<std::pair<T,bool>(
		const Rcpp::Vector<RTYPE>&,
		const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int n = X.nrow();
	unsigned int N_bdd = n*n;

	csc_mat<T> out(n, n);
	out.i.resize(0);
	out.x.resize(0);
	out.p.assign(n+1, N_bdd);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i <= j; i++) {
			const std::pair<T,bool>& f_ij = f(X.row(i), X.row(j));
			const T& val = f_ij.first;
			bool ind = f_ij.second;
			if (ind) {
				if (out.p[j] == N_bdd) {
					out.p[j] = out.x.size();
				}
				out.i.push_back(i);
				out.x.push_back(val);
			}
		}
	}

	// Handle pointer for last column and any empty columns
	out.p[n] = out.x.size();
	for (int j = n-1; j >= 0; j--) {
		out.p[j] = std::min(out.p[j], out.p[j+1]);
	}

	return out;
}

template <typename T, int RTYPE>
inline csc_mat<T> outer_sp(
	const Rcpp::Matrix<RTYPE>& X,
	const Rcpp::Matrix<RTYPE>& Y,
	const std::function<T,bool>(
		const Rcpp::Vector<RTYPE>&,
		const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int m = X.nrow();
	unsigned int n = Y.nrow();
	unsigned int N_bdd = m*n;

	csc_mat<T> out(m, n);
	out.i.resize(0);
	out.x.resize(0);
	out.p.assign(n+1, N_bdd);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			const std::pair<T,bool>& f_ij = f(X.row(i), X.row(j));
			const T& val = f_ij.first;
			bool ind = f_ij.second;

			if (ind) {
				if (out.p[j] == N_bdd) {
					out.p[j] = out.x.size();
				}
				out.i.push_back(i);
				out.x.push_back(val);
			}
		}
	}

	// Handle pointer for last column and any empty columns
	out.p[n] = out.x.size();
	for (int j = n-1; j >= 0; j--) {
		out.p[j] = std::min(out.p[j], out.p[j+1]);
	}

	return out;
}

template <int RTYPE>
inline Rcpp::NumericVector outer_matvec(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<double(
		const Rcpp::Vector<RTYPE>&,
		const Rcpp::Vector<RTYPE>&)>& f,
	const Rcpp::NumericVector& a)
{
	unsigned int n = X.nrow();
	if (n != a.size()) {
		Rcpp::stop("Dimension mismatch");
	}

	Rcpp::NumericVector out(n, 0);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < j; i++) {
			double f_ij = f(X.row(i), X.row(j));
			double f_ji = f_ij;
			out(i) += f_ij * a(j);
			out(j) += f_ji * a(i);
		}
	}

	for (unsigned int i = 0; i < n; i++) {
		double f_ii = f(X.row(i), X.row(i));
		out(i) += f_ii * a(i);
	}

	return out;
}

template <int RTYPE>
inline Rcpp::NumericVector outer_matvec(
	const Rcpp::Matrix<RTYPE>& X,
	const Rcpp::Matrix<RTYPE>& Y,
	const std::function<double(
		const Rcpp::Vector<RTYPE>&,
		const Rcpp::Vector<RTYPE>&)>& f,
	const Rcpp::NumericVector& a)
{
	unsigned int m = X.nrow();
	unsigned int n = Y.nrow();

	if (n != a.size()) {
		Rcpp::stop("Dimension mismatch");
	}

	Rcpp::NumericVector out(m, 0);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			double f_ij = f(X.row(i), Y.row(j));
			out(i) += f_ij * a(j);
		}
	}

	return out;
}

/*
* More general versions of outer functions
*/

template <typename S, typename E>
inline mat<E> outer(
	const std::vector<S>& x,
	const std::function<E(const S&, const S&)>& f)
{
	unsigned int n = x.size();
	mat<E> out(n, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < n; i++) {
			out.x[i + j*n] = f(x[i], x[j]);
		}
	}

	return out;
}

template <typename S, typename T, typename E>
inline mat<E> outer(
	const std::vector<S>& x,
	const std::vector<T>& y,
	const std::function<E(const S&, const T&)>& f)
{
	unsigned int m = x.size();
	unsigned int n = y.size();
	mat<E> out(m, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < n; i++) {
			out.x[i + j*n] = f(x[i], y[j]);
		}
	}

	return out;
}

template <typename S, typename E>
inline csc_mat<E> outer_sp(
	const std::vector<S>& x,
	const std::function<E,bool>(
		const std::vector<S>&,
		const std::vector<S>&)>& f)
{
	unsigned int n = x.size();
	unsigned int N_bdd = n*n;

	csc_mat<E> out(n, n);
	out.i.resize();
	out.x.resize();
	out.p.assign(n+1, N_bdd);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i <= j; i++) {
			const std::pair<T,bool>& f_ij = f(X.row(i), X.row(j));
			const T& val = f_ij.first;
			bool ind = f_ij.second;

			if (ind) {
				if (out.p[j] == N_bdd) {
					out.p[j] = out.x.size();
				}
				out.i.push_back(i);
				out.x.push_back(val);
			}
		}
	}

	// Handle pointer for last column and any empty columns
	out.p[n] = out.x.size();
	for (int j = n-1; j >= 0; j--) {
		out.p[j] = std::min(out.p[j], out.p[j+1]);
	}

	return out;
}

template <typename S, typename T, typename E>
inline csc_mat<E> outer_sp(
	const std::vector<S>& x,
	const std::vector<T>& y,
	const std::function<E,bool>(
		const std::vector<S>&,
		const std::vector<T>&)>& f)
{
	unsigned int m = x.size();
	unsigned int n = y.size();
	unsigned int N_bdd = m*n;

	csc_mat<E> out(m, n);
	out.i.resize();
	out.x.resize();
	out.p.assign(n+1, N_bdd);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			const std::pair<T,bool>& f_ij = f(X.row(i), X.row(j));
			const T& val = f_ij.first;
			bool ind = f_ij.second;

			if (ind) {
				if (out.p[j] == N_bdd) {
					out.p[j] = out.x.size();
				}
				out.i.push_back(i);
				out.x.push_back(val);
			}
		}
	}

	// Handle pointer for last column and any empty columns
	out.p[n] = out.x.size();
	for (int j = n-1; j >= 0; j--) {
		out.p[j] = std::min(out.p[j], out.p[j+1]);
	}

	return out;
}

template <typename S, typename E>
inline std::vector<E> outer_matvec(
	const std::vector<S>& x,
	const std::function<E(const std::vector<S>&, const std::vector<S>&)>& f,
	const std::vector<double>& a)
{
	unsigned int n = x.size();

	if (a.size() != n) {
		Rcpp::stop("Dimension mismatch: dim(a) = %d and dim(x) = %d", a.size(), n);
	}

	std::vector<E> out(n);

	for (unsigned int j = 0; j < n; j++) {
		out[j] = a[0] * f(x[0], x[j]);
		for (unsigned int i = 1; i < n; i++) {
			out[j] += a[i] * f(x[i], x[j]);
		}
	}

	return out;
}

template <typename S, typename T, typename E>
inline std::vector<E> outer_matvec(
	const std::vector<S>& x,
	const std::vector<T>& y,
	const std::function<E(const std::vector<S>&, const std::vector<T>&)>& f,
	const std::vector<double>& a)
{
	unsigned int m = x.size();
	unsigned int n = y.size();

	if (a.size() != n) {
		Rcpp::stop("Dimension mismatch: dim(a) = %d and dim(y) = %d", a.size(), n);
	}

	std::vector<E> out(m);

	for (unsigned int j = 0; j < n; j++) {
		out[j] = a[0] * f(x[0], y[j]);
		for (unsigned int i = 1; i < n; i++) {
			out[j] += a[i] * f(x[i], y[j]);
		}
	}

	return out;
}

}

#endif

