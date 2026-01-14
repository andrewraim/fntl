#ifndef FNTL_COO_MAT_H
#define FNTL_COO_MAT_H

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
**/

/*
* Components defined in this file are defined in a particular way to support
* the `as` and `wrap` operations.
*/

#include <RcppCommon.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

namespace fntl {

template <typename T>
struct coo_mat
{
	std::vector<unsigned int> i;
	std::vector<unsigned int> j;
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	coo_mat() { };
	coo_mat(unsigned int rows, unsigned int cols) : m(rows), n(cols) { };
	coo_mat(SEXP obj);
	operator SEXP() const;
};

}

#include <Rcpp.h>
#include "mat.h"

namespace fntl {

/*
* Constructors from SEXP objects
*/

template <typename T>
inline coo_mat<T>::coo_mat(SEXP obj)
{
	const Rcpp::List& list = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "i", "j", "x", "m", "n" };
	const Rcpp::StringVector& ac_names = list.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	std::vector<T> xx = list["x"];

	i = x.containsElementNamed("i") ? list["i"] : i;
	j = x.containsElementNamed("j") ? list["j"] : j;
	x = x.containsElementNamed("x") ? xx : x;
	m = x.containsElementNamed("m") ? list["m"] : m;
	n = x.containsElementNamed("n") ? list["n"] : n;
}

/*
* Conversion operators to SEXP objects
*/

template <typename T>
inline coo_mat<T>::operator SEXP() const
{
	Rcpp::IntegerVector ii(i.begin(), i.end());
	Rcpp::IntegerVector jj(j.begin(), j.end());

	return Rcpp::List::create(
		Rcpp::Named("i") = ii + 1,
		Rcpp::Named("j") = jj + 1,
		Rcpp::Named("x") = x,
		Rcpp::Named("m") = m,
		Rcpp::Named("n") = n
	);
}

}

#endif

