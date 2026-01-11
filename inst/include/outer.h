#ifndef FNTL_OUTER_H
#define FNTL_OUTER_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

namespace fntl {

template <typename T, int RTYPE>
inline Rcpp::Matrix<RTYPE> outer(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& f)
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
	const std::function<T(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& f)
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
	const std::function<T(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& f,
	const std::function<bool(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& g)
{
	unsigned int n = X.nrow();
	unsigned int N = n*n;

	csc_mat<T> out;
	out.m = n;
	out.n = n;
	out.p.resize(n+1, N);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i <= j; i++) {
			double val = f(X.row(i), X.row(j));
			bool ind = g(X.row(i), X.row(j));
			if (ind) {
				if (out.p[j] == N) {
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
		if (out.p[j] == N) {
			out.p[j] = out.p[j+1];
		}
	}

	return out;
}

template <typename T, int RTYPE>
inline csc_mat<T> outer_sp(
	const Rcpp::Matrix<RTYPE>& X,
	const Rcpp::Matrix<RTYPE>& Y,
	const std::function<T(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& f,
	const std::function<bool(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& g)
{
	unsigned int m = X.nrow();
	unsigned int n = Y.nrow();
	unsigned int N = m*n;

	csc_mat<T> out;
	out.p.resize(n+1, N);
	out.m = m;
	out.n = n;

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			double val = f(X.row(i), Y.row(j));
			bool ind = g(X.row(i), Y.row(j));
			if (ind) {
				if (out.p[j] == N) {
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
		if (out.p[j] == N) {
			out.p[j] = out.p[j+1];
		}
	}

	return out;
}

template <int RTYPE>
inline Rcpp::NumericVector outer_matvec(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<double(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& f,
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
	const std::function<double(const Rcpp::Vector<RTYPE>&, const Rcpp::Vector<RTYPE>&)>& f,
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

}

#endif

