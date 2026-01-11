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
struct mat
{
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	mat() { };
};


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
		Rcpp::Named("i") = ii + 1,
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
		Rcpp::Named("j") = jj + 1,
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
		Rcpp::Named("i") = ii + 1,
		Rcpp::Named("j") = jj + 1,
		Rcpp::Named("x") = x,
		Rcpp::Named("m") = m,
		Rcpp::Named("n") = n
	);
}

/*
* Conversion operators between matrix types
*/


template <typename T, int RTYPE>
inline csc_mat<T> to_csc(const Rcpp::Matrix<RTYPE>& x,
	const std::function<bool(const T&)>&f)
{
	unsigned int m = x.nrow();
	unsigned int n = x.ncol();
	unsigned int N = m*n;

	csc_mat<T> out;
	out.p.resize(n+1, N);
	out.m = m;
	out.n = n;

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			bool ind = f(x(i,j));
			if (ind) {
				if (out.p[j] == N) {
					out.p[j] = out.x.size();
				}
				out.i.push_back(i);
				out.x.push_back(x(i,j));
			}
		}
	}

	// Handle last pointer and any empty columns
	out.p[n] = out.x.size();
	for (int j = n-1; j >= 0; j--) {
		if (out.p[j] == N) {
			out.p[j] = out.p[j+1];
		}
	}

	return out;
}


template <typename T, int RTYPE>
inline Rcpp::Matrix<RTYPE> to_Matrix(const csc_mat<T>& x)
{
	unsigned int m = x.m;
	unsigned int n = x.n;
	unsigned int N = x.p[n];

	Rcpp::Matrix<RTYPE> out(m, n);

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
inline csr_mat<T> to_csr(const csc_mat<T>& x)
{
	unsigned int m = x.m;
	unsigned int n = x.n;
	unsigned int N = x.p[n];

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

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int l = x.p[j]; l < x.p[j+1]; l++) {
			unsigned int i = x.i[l];
			coord_t c;
			c.first = i;
			c.second = j;
			z[c] = x.x[l];
		}
	}

	csr_mat<T> out;
	out.m = m;
	out.n = n;
	out.p.resize(m+1, N);

	auto itr = z.begin();
	for (; itr != z.end(); ++itr) {
		const coord_t& c = itr->first;
		const T& v = itr->second;
		unsigned int i = c.first;
		unsigned int j = c.second;

		if (out.p[i] == N) {
			out.p[i] = out.x.size();
		}
		out.j.push_back(j);
		out.x.push_back(v);
	}

	for (int i = m-1; i >= 0; i--) {
		out.p[i] = std::min(out.p[i], out.p[i+1]);
	}

	return out;
}

template <typename T>
inline coo_mat<T> to_coo(const csc_mat<T>& x)
{
	unsigned int m = x.m;
	unsigned int n = x.n;
	unsigned int N = x.p[n];

	coo_mat<T> out;
	out.m = m;
	out.n = n;

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int l = x.p[j]; l < x.p[j+1]; l++) {
			out.i.push_back(x.i[l]);
			out.j.push_back(j);
			out.x.push_back(x.x[l]);
		}
	}

	return out;
}

}

#endif

