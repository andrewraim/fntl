#ifndef FNTL_MAT_H
#define FNTL_MAT_H

#include <complex>
#include <RcppCommon.h>

/*
* Components defined in this file are defined in a particular way to support
* the `as` and `wrap` operations.
*/

namespace fntl {

template <typename T>
struct mat
{
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	mat() { };
	mat(unsigned int rows, unsigned int cols) : m(rows), n(cols), x(rows*cols) { };

	// Construct from an Rcpp matrix type
	template <int RTYPE>
	mat(const Rcpp::Matrix<RTYPE>& obj);

	// Serialize to an S-expression.
	operator SEXP() const;

	// Access an element via x(i,j). No searching required here. Check
	// bounds and throw an exception if attempting to go outside allocated
	// memory. First function allows element to be modified and second ensures
	// that it cannot be (const).
	T& operator()(unsigned int row, unsigned int col);
	const T& operator()(unsigned int row, unsigned int col) const;
};

template <typename T>
inline T& mat<T>::operator()(unsigned int row, unsigned int col)
{
	Rprintf("mat: size of x is %d\n", x.size());
	if (row >= m || col >= n) {
		 Rcpp::stop("Index out of bounds");
	}
	return x[row + col*m];
}

template <typename T>
inline const T& mat<T>::operator()(unsigned int row, unsigned int col) const
{
	Rprintf("mat: size of x is %d\n", x.size());
	if (row >= m || col >= n) {
		 Rcpp::stop("Index out of bounds");
	}
	return x[row + col*m];
}

template <typename T>
template <int RTYPE>
inline mat<T>::mat(const Rcpp::Matrix<RTYPE>& y)
{
	m = y.nrow();
	n = y.ncol();
	x.assign(y.begin(), y.end());
}

}

#include <Rcpp.h>
#include "mat.h"

namespace fntl {

/*
* Conversion operators to SEXP objects
*/

template<>
inline mat<double>::operator SEXP() const
{
	Rcpp::NumericVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<int>::operator SEXP() const
{
	Rcpp::IntegerVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<bool>::operator SEXP() const
{
	Rcpp::LogicalVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<std::complex<double>>::operator SEXP() const
{
	Rcpp::ComplexVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<std::string>::operator SEXP() const
{
	Rcpp::CharacterVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

}

#endif
