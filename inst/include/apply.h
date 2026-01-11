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

}

#endif
