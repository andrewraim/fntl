#ifndef FNTL_APPLY_H
#define FNTL_APPLY_H

#include <Rcpp.h>
#include "mat.h"
#include "csc-mat-builder.h"
#include "mdarray.h"

namespace fntl {

template <typename T, int RTYPE>
std::vector<T> row_apply(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int m = X.nrow();
	std::vector<T> out(m);

	for (unsigned int i = 0; i < m; i++) {
		const Rcpp::ConstMatrixRow<RTYPE>& xx = X.row(i);
		out[i] = f(xx);
	}

	return out;
}

template <typename T, int RTYPE>
std::vector<T> col_apply(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(const Rcpp::Vector<RTYPE>&)>& f)
{
	unsigned int n = X.ncol();
	std::vector<T> out(n);

	for (unsigned int i = 0; i < n; i++) {
		const Rcpp::ConstMatrixColumn<RTYPE>& xx = X.column(i);
		out[i] = f(xx);
	}

	return out;
}

template <typename S, typename T, int RTYPE>
mat<T> mat_apply(
	const Rcpp::Matrix<RTYPE>& X,
	const std::function<T(S)>& f)
{
	unsigned int m = X.nrow();
	unsigned int n = X.ncol();
	mat<T> out(m, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			out(i,j) = f(X(i,j));
		}
	}

	return out;
}

/*
* More general apply for dense matrices. This version can produce vectors and
* matrices of a different type than the argument. For example, with input as
* a matrix of strings, the output can be a matrix of string lengths.
*/

template <typename S, typename T>
std::vector<T> row_apply(
	const mat<S>& X,
	const std::function<T(const std::vector<S>&)>& f)
{
	unsigned int m = X.m;
	unsigned int n = X.n;

	std::vector<S> x(n);
	std::vector<T> out(m);

	for (unsigned int i = 0; i < m; i++) {
		for (unsigned int j = 0; j < n; j++) {
			x[j] = X(i,j);
		}
		out[i] = f(x);
	}

	return out;
}

template <typename S, typename T>
std::vector<T> col_apply(
	const mat<S>& X,
	const std::function<T(const std::vector<S>&)>& f)
{
	unsigned int m = X.m;
	unsigned int n = X.n;

	std::vector<S> x(m);
	std::vector<T> out(n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			x[i] = X(i,j);
		}
		out[j] = f(x);
	}

	return out;
}

template <typename S, typename T>
mat<T> mat_apply(
	const mat<S>& X,
	const std::function<T(const S&)>& f)
{
	unsigned int m = X.m;
	unsigned int n = X.n;
	mat<T> out(m, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			out(i,j) = f(X(i,j));
		}
	}

	return out;
}

/*
* Apply for sparse matrices. Be able to transform from one type of matrix to
* another. Row and col apply transform to an STL vector.
*/

template <typename S, typename T>
std::vector<T> row_apply(
	const csc_mat<S>& X,
	const std::function<T(const std::vector<S>&,
		const std::vector<unsigned int>&)>& f)
{
	const csc_mat<T>& Y = to_csr(X);
	unsigned int m = Y.m;
	unsigned int n = Y.n;
	unsigned int N = Y.p[n];
	std::vector<T> out(m);

	for (unsigned int i = 0; i < m; i++) {
		std::vector<S> y;
		std::vector<unsigned int> idx;
		for (unsigned int l = Y.p[i]; l < Y.p[i+1]; l++) {
			unsigned int j = Y.j[l];
			const S& v = Y.x[l];
			y.push_back(v);
			idx.push_back(i);
		}
		out[i] = f(y, idx);
	}

	return out;
}

template <typename S, typename T>
std::vector<T> col_apply(
	const csc_mat<S>& X,
	const std::function<T(const std::vector<S>&,
		const std::vector<unsigned int>&)>& f)
{
	unsigned int m = X.m;
	unsigned int n = X.n;
	unsigned int N = X.p[n];
	std::vector<T> out(n);

	for (unsigned int j = 0; j < n; j++) {
		std::vector<S> x;
		std::vector<unsigned int> idx;
		for (unsigned int l = X.p[j]; l < X.p[j+1]; l++) {
			unsigned int i = X.i[l];
			const S& v = X.x[l];
			x.push_back(v);
			idx.push_back(i);
		}
		out[j] = f(x, idx);
	}

	return out;
}

/*
* TBD: consider modifying this so that `f` also returns a bool so that we can
* make the result more sparse than the input. Or keep it simple and avoid
* modifying the sparseness here?
*/

template <typename S, typename T>
csc_mat<T> mat_apply(
	const csc_mat<S>& X,
	const std::function<T(const S&)>& f)
{
	unsigned int m = X.m;
	unsigned int n = X.n;
	csc_mat_builder<T> builder(m, n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int l = X.p[j]; l < X.p[j+1]; l++) {
			unsigned int i = X.i[l];
			const S& v = X.x[l];
			builder.set(i, j, v);
		}
	}

	return builder.get();
}

template <typename T>
std::vector<T> coord_apply(
	unsigned int n,
	const std::function<T(unsigned int)>& f)
{
	std::vector<T> out(n);

	for (unsigned int i = 0; i < n; i++) {
		out[i] = f(i);
	}

	return out;
}

template <typename T>
mat<T> coord_apply(
	unsigned int m,
	unsigned int n,
	const std::function<T(unsigned int, unsigned int)>& f)
{
	mat<T> out(m,n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			out(i,j) = f(i,j);
		}
	}

	return out;
}

template <typename T>
csc_mat<T> coord_apply_sp(
	unsigned int m,
	unsigned int n,
	const std::function<std::pair<T,bool>(unsigned int, unsigned int)>& f)
{
	csc_mat_builder<T> builder(m,n);

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			const std::pair<T,bool>& f_ij = f(i,j);
			const T& val = f_ij.first;
			bool ind = f_ij.second;
			if (ind) {
				builder.set(i, j, val);
			}
		}
	}

	return builder.get();
}














template<typename S, typename T, std::size_t N, std::size_t M>
mdarray<T,M> apply(
	const mdarray<S,N>& x,
	const coord_t<M> margins,
	const std::function<T(const mdarray<S,N-M>&)>& f)
{
	// Get the non-margins
	// Iterate through each level of the margins
	// For each level, collect the non-margins and put them into a sub-mdarray
	// Pass this sub-mdarray to f and save the result into out.

	const Rcpp::IntegerVector& dims_vec = Rcpp::wrap(x.dim());
	const Rcpp::IntegerVector& margins_vec = Rcpp::wrap(margins);
	const Rcpp::IntegerVector& nonmargins_vec = Rcpp::setdiff(dims_vec, margins_vec);

	mdarray<T,M> out(margins);

	// TBD
	//
	// We need an index for the margins and be able to increment through its
	// exents. For each level, we need to increment over all the non-margins.
	// I think this might require changing coord_t to a class. This could
	// include from_coord_list somehow.
	//
	// This might end up being a bit inefficient; we are creating many copies
	// of the non-margins. This might be avoidable with mdspan in STL, but that
	// requires a version of C++ that may not be available on all systems.
	//
	// We can avoid making a copy if we can pass an iterator that iterates over
	// the nonmargins with the margins fixed. The user would need to interact
	// with this.

	coord_t<M> idx_margins;
	coord_t<N-M> idx_nonmargins;

	for (unsigned int l = 0; l < margins_vec.size(); l++) {

	}

	return mdarray<T,M>(margins);
}





/*
**** TBD: Apply a function of indices ****

# Sparse Apply Modification

Change the function f to return a pair of `T` and `bool`. Remove `g` from the
interface. This is for `apply` and `outer` functions that return sparse
matrices.

# Apply functions for indices

Here is pseudo-code for how I think it could work

```r
x = rnorm(5)
f = \(i,j) { cumsum(x)[i] * cumsum(x)[j] }
X = matrix(NA, 5, 5)
for (i in 1:5) {
	for (j in 1:5) {
		X[i,j] = f(i,j)
	}
}
idx_apply(X, f)
```

We also want to be able to produce a sparse matrix. This would involve
modifying `f` to return a boolean as well.

```r
f = \(i, j) {
	out = list(
		value = cumsum(x)[i] * cumsum(x)[j],
		ind = abs(i-j) <= 1
	}
}
idx_apply_sp(X, f)
```

R can handle this with `outer` using a call like the following.
(<https://stackoverflow.com/a/7395664>)

```r
outer(1:nrow(mat), 1:ncol(mat) , FUN = function(r,c) log(r+c) )
```

We can accomplish this with calls to our `outer` functions in the same way.
*/

}

#endif

