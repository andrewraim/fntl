#ifndef FNTL_CSC_MAT_H
#define FNTL_CSC_MAT_H

/** TBD
* MAYBE THESE SHOULD NOT BE SERIALIZABLE DIRECTLY TO AND FROM R
* We do want to be able to convert from Rcpp objects, and we want to convert
* to representations that can be passed back to R, but we do not
* necessarily want to be accepting these from R.
*
* For mat: be able to switch back and forth between supported Rcpp matrix
* formats. It can be used as a container for other kinds of data, but these
* will not be readily converted to Rcpp.
*
* For spmat, be able to save structures to an Rcpp List. Also be able to
* convert from an Rcpp dense matrix or a mat.
*
*/

/*
* Components defined in this file are defined in a particular way to support
* the `as` and `wrap` operations.
*/

#include <RcppCommon.h>

namespace fntl {

// Forward declarations; don't include their headers until Rcpp.h is included.
template <typename T> struct csr_mat;
template <typename T> struct coo_mat;
template <typename T> struct csc_mat_builder;

template <typename T>
struct csc_mat
{
/* Constructors */
	csc_mat();
	csc_mat(unsigned int rows, unsigned int cols);

	csc_mat(SEXP obj);

	template <int RTYPE>
	csc_mat(const Rcpp::Matrix<RTYPE>& x, const std::function<bool(const T&)>&f);

	template <typename S>
	csc_mat(const csc_mat<S>& y, const std::function<bool(const S&)>&f);

/* Serialize to S-expression */
	operator SEXP() const;

/* Convert to other matrix format */
	coo_mat<T> to_coo() const;
	csr_mat<T> to_csr() const;

	template <int RTYPE>
	Rcpp::Matrix<RTYPE> to_Matrix() const;

/* Member variables */
	unsigned int m;
	unsigned int n;
	std::vector<unsigned int> i;
	std::vector<unsigned int> p;
	std::vector<T> x;
};

typedef csc_mat<double> csc_dmat;

}

#include <Rcpp.h>
//#include "typedefs.h"
//#include "typedefs-rcpp.h"
#include "util.h"
#include "mat.h"
#include "csr-mat.h"
#include "coo-mat.h"
#include "csc-mat-builder.h"
#include "csr-mat-builder.h"

namespace fntl {

/*
* Constructors
*/

template <typename T>
csc_mat<T>::csc_mat()
: m(0), n(0), i(), p(1), x()
{
}

template <typename T>
csc_mat<T>::csc_mat(unsigned int rows, unsigned int cols)
: m(rows), n(cols), i(), p(cols+1), x()
{
}

template <typename T>
template <int RTYPE>
csc_mat<T>::csc_mat(const Rcpp::Matrix<RTYPE>& y,
	const std::function<bool(const T&)>&f)
{
	m = y.nrow();
	n = y.ncol();
	csc_mat_builder<T> builder(m, n);

	for (unsigned int jj = 0; jj < n; jj++) {
		for (unsigned int ii = 0; ii < m; ii++) {
			bool ind = f(y(ii,jj));
			if (ind) {
				builder.set(ii, jj, y(ii,jj));
			}
		}
	}

	const csc_mat<T>& out = builder.get();
	i = out.i;
	p = out.p;
	x = out.x;
}

template <typename T>
template <typename S>
csc_mat<T>::csc_mat(const csc_mat<S>& y, const std::function<bool(const S&)>&f)
{
	m = y.m;
	n = y.n;
	csc_mat_builder<T> builder(m, n);

	for (unsigned int jj = 0; jj < n; jj++) {
		for (unsigned int ii = 0; ii < m; ii++) {
			bool ind = f(y(ii,jj));
			if (ind) {
				builder.set(ii, jj, y(ii,jj));
			}
		}
	}

	const csc_mat<T>& out = builder.get();
	i = out.i;
	p = out.p;
	x = out.x;
}


/*
* Constructors from SEXP objects
*/

template <typename T>
inline csc_mat<T>::csc_mat(SEXP obj)
{
	const Rcpp::List& list = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "i", "p", "x", "m", "n" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	std::vector<T> xx = list["x"];

	i = list.containsElementNamed("i") ? list["i"] : i;
	p = list.containsElementNamed("p") ? list["p"] : p;
	x = list.containsElementNamed("x") ? xx : x;
	m = list.containsElementNamed("m") ? list["m"] : m;
	n = list.containsElementNamed("n") ? list["n"] : n;
}

/*
* Conversion operator to SEXP object
*/

template <typename T>
inline csc_mat<T>::operator SEXP() const
{
	Rcpp::IntegerVector ii(i.begin(), i.end());
	Rcpp::IntegerVector pp(p.begin(), p.end());

	return Rcpp::List::create(
		Rcpp::Named("i") = ii + 1,
		Rcpp::Named("p") = pp,
		Rcpp::Named("x") = x,
		Rcpp::Named("m") = m,
		Rcpp::Named("n") = n
	);
}

/*
* Conversion between matrix types
*/

template <typename T>
template <int RTYPE>
inline Rcpp::Matrix<RTYPE> csc_mat<T>::to_Matrix() const
{
	unsigned int m = m;
	unsigned int n = n;
	unsigned int N = p[n];

	Rcpp::Matrix<RTYPE> out(m, n);

	for (unsigned int jj = 0; jj < n; jj++) {
		for (unsigned int l = p[jj]; l < p[jj+1]; l++) {
			unsigned int ii = i[l];
			unsigned int idx = ii + jj*m;
			out(ii,jj) = x[l];
		}
	}

	return out;
}

template <typename T>
inline csr_mat<T> csc_mat<T>::to_csr() const
{
	csr_mat_builder<T> builder(m, n);

	for (unsigned int jj = 0; jj < n; jj++) {
		for (unsigned int l = p[jj]; l < p[jj+1]; l++) {
			unsigned int ii = i[l];
			builder.set(ii, jj, x[l]);
		}
	}

	return builder.get();
}

template <typename T>
inline coo_mat<T> csc_mat<T>::to_coo() const
{
	coo_mat<T> out(m, n);

	for (unsigned int jj = 0; jj < n; jj++) {
		for (unsigned int l = p[jj]; l < p[jj+1]; l++) {
			out.i.push_back(i[l]);
			out.j.push_back(jj);
			out.x.push_back(x[l]);
		}
	}

	return out;
}

}

#endif

