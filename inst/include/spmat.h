#ifndef FNTL_SPMAT_H
#define FNTL_SPMAT_H

/*
* This code follows a specific structure so that we can use the `as` and `wrap`
* constructs to serialize between structs and Rcpp Lists. The structs are
* defined first without Rcpp included yet, then Rcpp is included and the
* implementations for serialization operations are give.
*
* The pattern we follow here is referred to as "intrusive" (rather than
* "non-intrusive") because `wrap` and `as` are defined via member functions.
*
* See the following articles:
* <https://gallery.rcpp.org/articles/custom-templated-wrap-and-as-for-seamingless-interfaces>
* <https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-extending.pdf>
*
* The issue also came up in Stack\ Overflow threads such as:
* <https://stackoverflow.com/questions/51110244/in-rcpp-how-to-get-a-user-defined-structure-from-c-into-r>
* <https://stackoverflow.com/questions/74887786/specialising-rcppas-for-stdarray>
*/

#include <RcppCommon.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

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
	csc_mat(SEXP obj);
	operator SEXP() const;
};

template <typename T>
struct csr_mat
{
	std::vector<unsigned int> j;
	std::vector<unsigned int> p;
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	csr_mat() { };
	csr_mat(SEXP obj);
	operator SEXP() const;
};

template <typename T>
struct coo_mat
{
	std::vector<unsigned int> i;
	std::vector<unsigned int> j;
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	coo_mat() { };
	coo_mat(SEXP obj);
	operator SEXP() const;
};

}

#include <Rcpp.h>

namespace fntl {

/*
* Constructors from SEXP objects
*/

template <typename T>
inline csc_mat<T>::csc_mat(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "i", "p", "x", "m", "n" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	std::vector<T> xx = x["x"];

	i = x.containsElementNamed("i") ? x["i"] : i;
	p = x.containsElementNamed("p") ? x["p"] : p;
	x = x.containsElementNamed("x") ? xx : x;
	m = x.containsElementNamed("m") ? x["m"] : m;
	n = x.containsElementNamed("n") ? x["n"] : n;
}

template <typename T>
inline csr_mat<T>::csr_mat(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "j", "p", "x", "m", "n" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	std::vector<T> xx = x["x"];

	j = x.containsElementNamed("j") ? x["j"] : j;
	p = x.containsElementNamed("p") ? x["p"] : p;
	x = x.containsElementNamed("x") ? xx : x;
	m = x.containsElementNamed("m") ? x["m"] : m;
	n = x.containsElementNamed("n") ? x["n"] : n;
}

template <typename T>
inline coo_mat<T>::coo_mat(SEXP obj)
{
	const Rcpp::List& x = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "i", "j", "x", "m", "n" };
	const Rcpp::StringVector& ac_names = x.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	std::vector<T> xx = x["x"];

	i = x.containsElementNamed("i") ? x["i"] : i;
	j = x.containsElementNamed("j") ? x["j"] : j;
	x = x.containsElementNamed("x") ? xx : x;
	m = x.containsElementNamed("m") ? x["m"] : m;
	n = x.containsElementNamed("n") ? x["n"] : n;
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
		Rcpp::Named("i") = ii,
		Rcpp::Named("p") = pp,
		Rcpp::Named("x") = x,
		Rcpp::Named("m") = m,
		Rcpp::Named("n") = n
	);
}

template <typename T>
inline csr_mat<T>::operator SEXP() const
{
	Rcpp::IntegerVector jj(j.begin(), j.end());
	Rcpp::IntegerVector pp(p.begin(), p.end());

	return Rcpp::List::create(
		Rcpp::Named("j") = jj,
		Rcpp::Named("p") = pp,
		Rcpp::Named("x") = x,
		Rcpp::Named("m") = m,
		Rcpp::Named("n") = n
	);
}

template <typename T>
inline coo_mat<T>::operator SEXP() const
{
	Rcpp::IntegerVector ii(i.begin(), i.end());
	Rcpp::IntegerVector jj(j.begin(), j.end());

	return Rcpp::List::create(
		Rcpp::Named("i") = ii,
		Rcpp::Named("j") = jj,
		Rcpp::Named("x") = x,
		Rcpp::Named("m") = m,
		Rcpp::Named("n") = n
	);
}

template <typename T, int RTYPE>
// inline Rcpp::Matrix<RTYPE> as(const csc_mat<T>& x, const T& s)
inline Rcpp::Matrix<RTYPE> as(const csc_mat<T>& x)
{
	unsigned int m = x.m;
	unsigned int n = x.n;
	unsigned int N = x.x[n];

	Rcpp::Matrix<RTYPE> out(m, n);
	//out.fill(s);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int l = x.p[j]; l < x.p[j+1]; l++) {
			unsigned int i = x.i[l];
			unsigned int idx = i + j*m;
			out(i,j) = x.x[l];
		}
	}

	return out;
}


template <typename T>
inline coo_mat<T> as(const csc_mat<T>& x)
{
	unsigned int m = x.m;
	unsigned int n = x.n;
	unsigned int N = x.x[n];

	coo_mat<T> out;
	out.m = m;
	out.n = n;

	unsigned int idx = 0;;

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int l = x.p[j]; l < x.p[j+1]; l++) {
			out.i.push_back(x.i[l]);
			out.j.push_back(j);
			out.x.push_back(x.x[l]);
			idx++;
		}
	}

	return out;
}


}

#endif
