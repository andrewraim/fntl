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
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"
#include "mat.h"
#include "csr-mat.h"
#include "coo-mat.h"

namespace fntl {

template <typename T>
struct csc_mat
{
	std::vector<unsigned int> i;
	std::vector<unsigned int> p;
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	csc_mat() { };
	csc_mat(unsigned int rows, unsigned int cols) : m(rows), n(cols) { };
	csc_mat(SEXP obj);

	template <int RTYPE>
	csc_mat(const Rcpp::Matrix<RTYPE>& x, const std::function<bool(const T&)>&f);

	operator SEXP() const;

	coo_mat<T> to_coo() const;
	csr_mat<T> to_csr() const;

	template <int RTYPE>
	Rcpp::Matrix<RTYPE> to_Matrix() const;
};

/*
* Constructors from dense matrices
*/

template <typename T>
template <int RTYPE>
csc_mat<T>::csc_mat(const Rcpp::Matrix<RTYPE>& y,
	const std::function<bool(const T&)>&f)
{
	m = y.nrow();
	n = y.ncol();
	unsigned int N = m*n;
	p.resize(n+1, N);

	for (unsigned int jj = 0; jj < n; jj++) {
		for (unsigned int ii = 0; ii < m; ii++) {
			bool ind = f(y(ii,jj));
			if (ind) {
				if (p[jj] == N) {
					p[jj] = x.size();
				}
				i.push_back(ii);
				x.push_back(y(ii,jj));
			}
		}
	}

	// Handle last pointer and any empty columns
	p[n] = x.size();
	for (int jj = n-1; jj >= 0; jj--) {
		if (p[jj] == N) {
			p[jj] = p[jj+1];
		}
	}
}

}

#include <Rcpp.h>
#include "mat.h"

namespace fntl {

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
* Conversion operators to SEXP objects
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
	unsigned int m = m;
	unsigned int n = n;
	unsigned int N = p[n];

	// Use an STL map to order entries of x in row-major order
	typedef std::pair<unsigned int, unsigned int> coord_t;
	std::function<bool(const coord_t&, const coord_t&)> cmp =
	[](const coord_t& a, const coord_t& b) -> bool {
		if (a.first == b.first) {
			return a.second < b.second;
		}
		return a.first < b.first;
	};
	std::map<coord_t, T, decltype(cmp)> z(cmp);

	for (unsigned int jj = 0; jj < n; jj++) {
		for (unsigned int l = p[jj]; l < p[jj+1]; l++) {
			unsigned int ii = i[l];
			coord_t c;
			c.first = ii;
			c.second = jj;
			z[c] = x[l];
		}
	}

	csr_mat<T> out(m, n);
	out.p.resize(m+1, N);

	auto itr = z.begin();
	for (; itr != z.end(); ++itr) {
		const coord_t& c = itr->first;
		const T& vv = itr->second;
		unsigned int ii = c.first;
		unsigned int jj = c.second;

		if (out.p[ii] == N) {
			out.p[ii] = out.x.size();
		}
		out.j.push_back(jj);
		out.x.push_back(vv);
	}

	for (int ii = m-1; ii >= 0; ii--) {
		out.p[ii] = std::min(out.p[ii], out.p[ii+1]);
	}

	return out;
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

