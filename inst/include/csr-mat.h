#ifndef FNTL_CSR_MAT_H
#define FNTL_CSR_MAT_H

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
* The issue also came up in Stack Overflow threads such as:
* <https://stackoverflow.com/questions/51110244/in-rcpp-how-to-get-a-user-defined-structure-from-c-into-r>
* <https://stackoverflow.com/questions/74887786/specialising-rcppas-for-stdarray>
*/

#include <RcppCommon.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

namespace fntl {

template <typename T>
struct csr_mat
{
	std::vector<unsigned int> j;
	std::vector<unsigned int> p;
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	csr_mat() { };
	csr_mat(unsigned int rows, unsigned int cols) : m(rows), n(cols) { };
	csr_mat(SEXP obj);
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
inline csr_mat<T>::csr_mat(SEXP obj)
{
	const Rcpp::List& list = Rcpp::as<Rcpp::List>(obj);

	const Rcpp::StringVector& ex_names = { "j", "p", "x", "m", "n" };
	const Rcpp::StringVector& ac_names = list.names();
	const auto& diff = Rcpp::setdiff(ac_names, ex_names);
	if (diff.size() > 0) {
		Rcpp::stop("Unexpected list entries: %s", paste(diff, ", "));
	}

	std::vector<T> xx = list["x"];

	j = x.containsElementNamed("j") ? list["j"] : j;
	p = x.containsElementNamed("p") ? list["p"] : p;
	x = x.containsElementNamed("x") ? xx : x;
	m = x.containsElementNamed("m") ? list["m"] : m;
	n = x.containsElementNamed("n") ? list["n"] : n;
}

/*
* Conversion operators to SEXP objects
*/

template <typename T>
inline csr_mat<T>::operator SEXP() const
{
	Rcpp::IntegerVector jj(j.begin(), j.end());
	Rcpp::IntegerVector pp(p.begin(), p.end());

	return Rcpp::List::create(
		Rcpp::Named("j") = jj + 1,
		Rcpp::Named("p") = pp,
		Rcpp::Named("x") = x,
		Rcpp::Named("m") = m,
		Rcpp::Named("n") = n
	);
}

}

#endif

