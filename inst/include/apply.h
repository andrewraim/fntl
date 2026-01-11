#ifndef FNTL_APPLY_H
#define FNTL_APPLY_H

#include <Rcpp.h>

namespace fntl {

template <typename T, int RTYPE>
Rcpp::Vector<RTYPE> row_apply(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int m = X.nrow();
	Rcpp::Vector<RTYPE> out(m);

	for (unsigned int i = 0; i < m; i++) {
		const Rcpp::ConstMatrixRow<RTYPE>& xx = X.row(i);
		out(i) = f(xx);
	}

	return out;
}

template <typename T, int RTYPE>
Rcpp::Vector<RTYPE> col_apply(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int n = X.ncol();
	Rcpp::Vector<RTYPE> out(n);

	for (unsigned int i = 0; i < n; i++) {
		const Rcpp::ConstMatrixColumn<RTYPE>& xx = X.column(i);
		out(i) = f(xx);
	}

	return out;
}

template <typename T, int RTYPE>
Rcpp::Matrix<RTYPE> mat_apply(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(T)>& f)
{
	unsigned int m = X.nrow();
	unsigned int n = X.ncol();
	Rcpp::Matrix<RTYPE> out(m, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			out(i,j) = f(X(i,j));
		}
	}

	return out;
}

template <typename T>
csc_mat<T> row_apply(
	const csc_mat<T>& X,
	const std::function<T(std::vector<T>, std::vector<unsigned int>)>& f)
{
	const csc_mat<T>& Y = to_csr(X);
	unsigned int m = Y.m;
	unsigned int n = Y.n;
	unsigned int N = Y.p[n];
	std::vector<T> out(m);

	for (unsigned int i = 0; i < m; i++) {
		std::vector<T> y;
		std::vector<unsigned int> idx;
		for (unsigned int l = Y.p[i]; l < Y.p[i+1]; l++) {
			unsigned int j = Y.j[l];
			const T& v = Y.x[l];
			y.push_back(v);
			idx.push_back(i);
		}
		out[i] = f(y, idx);
	}

	return out;
}

template <typename T>
csc_mat<T> col_apply(
	const csc_mat<T>& X,
	const std::function<T(std::vector<T>, std::vector<unsigned int>)>& f)
{
	unsigned int m = X.m;
	unsigned int n = X.n;
	unsigned int N = X.p[n];
	std::vector<T> out(n);

	for (unsigned int j = 0; j < n; j++) {
		std::vector<T> x;
		std::vector<unsigned int> idx;
		for (unsigned int l = X.p[j]; l < X.p[j+1]; l++) {
			unsigned int i = X.i[l];
			const T& v = X.x[l];
			x.push_back(v);
			idx.push_back(i);
		}
		out[j] = f(x, idx);
	}

	return out;
}

template <typename T>
csc_mat<T> mat_apply(
	const csc_mat<T>& X,
	const std::function<T(T)>& f)
{
	unsigned int m = X.m;
	unsigned int n = X.n;
	unsigned int N = X.p[n];

	csc_mat<T> out;
	out.m = m;
	out.n = n;
	out.i = X.i;
	out.p = X.p;
	out.x.resize(N);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int l = X.p[j]; l < X.p[j+1]; l++) {
			unsigned int i = X.i[l];
			out.x[l] = f(X.x[l]);
		}
	}

	return out;
}

}

#endif
