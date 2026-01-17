#ifndef FNTL_MDARRAY_H
#define FNTL_MDARRAY_H

#include <RcppCommon.h>

/*
* Components defined in this file are defined in a particular way to support
* the `as` and `wrap` operations.
*/

namespace fntl {

template<std::size_t N>
coord_t<N> from_coord_list(const coord_list_t& idx)
{
	coord_t<N> out;
	std::copy(idx.begin(), idx.end(), out.begin());
	return out;
}

template<typename T, std::size_t N>
class mdarray {
public:
/* Constructors from Rcpp vector and from dimensions */
	template <int RTYPE>
	mdarray(const Rcpp::Vector<RTYPE>& obj);

	mdarray(const coord_t<N>& dim);
	mdarray(const coord_t<N>& dim, const T& fill);

	mdarray(const coord_list_t& dim);
	mdarray(const coord_list_t& dim, const T& fill);

/* Access elements */
	T& operator()(const coord_t<N>& idx);
	const T& operator()(const coord_t<N>& idx) const;

	T& operator()(const coord_list_t& idx);
	const T& operator()(const coord_list_t& idx) const;

/* Serialize to S-expression */
	// template<int RTYPE>
	// operator SEXP() const;
	template<int RTYPE>
	Rcpp::Vector<RTYPE> to_Vector() const;

/* Access dimensions and flat data */
	const coord_t<N>& dim() const { return _dim; }
	const std::vector<T>& data() const { return _elements; }
	std::vector<T>& data() { return _elements; }

private:
/* Flatten N-dimensional index to single index */
	unsigned int flatten(const coord_t<N>& idx) const;
	unsigned int flatten(const coord_list_t& idx) const;

	coord_t<N> _dim;
	std::vector<T> _elements;
};

template<typename T, std::size_t N>
mdarray<T,N>::mdarray(const coord_t<N>& dim)
: _dim(dim), _elements()
{
	unsigned int len = 1;

	for (unsigned int i = 0; i < _dim.size(); i++) {
		len *= _dim[i];
	}

	_elements.resize(len);
}

template<typename T, std::size_t N>
mdarray<T,N>::mdarray(const coord_t<N>& dim, const T& fill)
: _dim(dim), _elements()
{
	unsigned int len = 1;

	for (unsigned int i = 0; i < _dim.size(); i++) {
		len *= _dim[i];
	}

	_elements.assign(len, fill);
}

template<typename T, std::size_t N>
mdarray<T,N>::mdarray(const std::initializer_list<unsigned int>& dim)
{
	std::copy(dim.begin(), dim.end(), _dim.begin());
	unsigned int len = 1;

	for (unsigned int i = 0; i < _dim.size(); i++) {
		len *= _dim[i];
	}

	_elements.resize(len);
}

template<typename T, std::size_t N>
mdarray<T,N>::mdarray(const std::initializer_list<unsigned int>& dim, const T& fill)
{
	std::copy(dim.begin(), dim.end(), _dim.begin());
	unsigned int len = 1;

	for (unsigned int i = 0; i < _dim.size(); i++) {
		len *= _dim[i];
	}

	_elements.assign(len, fill);
}

template<typename T, std::size_t N>
template <int RTYPE>
inline mdarray<T,N>::mdarray(const Rcpp::Vector<RTYPE>& y)
{
	_elements.assign(y.begin(), y.end());

	const Rcpp::Dimension& dim = y.attr("dim");
	for (unsigned int i = 0; i < dim.size(); i++) {
		_dim[i] = dim[i];
	}
}

template<typename T, std::size_t N>
unsigned int mdarray<T,N>::flatten(const coord_t<N>& idx) const
{
	unsigned int out = 0;
	unsigned int b = 1;

	for (int i = 0; i < N; i++) {
		if (idx[i] >= _dim[i]) {
			Rcpp::stop("Index exceeds dimension");
		}

		out += b * idx[i];
		b *= _dim[i];
	}

	return out;
}

template<typename T, std::size_t N>
T& mdarray<T,N>::operator()(const coord_t<N>& idx)
{
	return _elements[flatten(idx)];
}

template<typename T, std::size_t N>
const T& mdarray<T,N>::operator()(const coord_t<N>& idx) const
{
	return _elements[flatten(idx)];
}

template<typename T, std::size_t N>
unsigned int mdarray<T,N>::flatten(const coord_list_t& idx) const
{
	return flatten(from_coord_list<N>(idx));
}

template<typename T, std::size_t N>
T& mdarray<T,N>::operator()(const coord_list_t& idx)
{
	return (*this)(from_coord_list<N>(idx));
}

template<typename T, std::size_t N>
const T& mdarray<T,N>::operator()(const coord_list_t& idx) const
{
	return (*this)(from_coord_list<N>(idx));
}

}

/*
* Conversion operators to SEXP objects
*/
/*
namespace Rcpp {
namespace traits {
	template<std::size_t N>
	SEXP wrap(const fntl::mdarray<double,N>& x) {
		Rcpp::NumericVector out = Rcpp::wrap(x.data());
		out.attr("dim") = x.dim();
		return out;
	}

	template<std::size_t N>
	SEXP wrap(const fntl::mdarray<int,N>& x) {
		Rcpp::IntegerVector out = Rcpp::wrap(x.data());
		out.attr("dim") = x.dim();
		return out;
	}
}
}
*/

#include <Rcpp.h>
#include <complex>
#include "mat.h"

namespace fntl {

template<typename T, std::size_t N>
template<int RTYPE>
inline Rcpp::Vector<RTYPE> mdarray<T,N>::to_Vector() const
{
	Rcpp::Vector<RTYPE> out(_elements.begin(), _elements.end());
	out.attr("dim") = _dim;
	return out;
}

/*
* Conversion operators to SEXP objects
*/

/*
template<typename T, std::size_t N>
template<int RTYPE>
inline mdarray<T,N>::operator SEXP() const
{
	Rcpp::Vector<RTYPE> out(_elements.begin(), _elements.end());
	out.attr("dim") = _dim;
	return out;
}
*/

/*
template<std::size_t N>
inline mdarray<double,N>::operator SEXP() const
{
	Rcpp::NumericVector out(_elements.begin(), _elements.end());
	out.attr("dim") = _dim;
	return out;
}

template<std::size_t N>
inline mdarray<int,N>::operator SEXP() const
{
	Rcpp::IntegerVector out = Rcpp::wrap(_elements);
	out.attr("dim") = Rcpp::Dimension(_dim);
	return out;
}

template<std::size_t N>
inline mdarray<bool,N>::operator SEXP() const
{
	Rcpp::LogicalVector out = Rcpp::wrap(_elements);
	out.attr("dim") = Rcpp::Dimension(_dim);
	return out;
}

template<std::size_t N>
inline mdarray<std::complex<double>,N>::operator SEXP() const
{
	Rcpp::ComplexVector out = Rcpp::wrap(_elements);
	out.attr("dim") = Rcpp::Dimension(_dim);
	return out;
}

template<std::size_t N>
inline mdarray<std::string,N>::operator SEXP() const
{
	Rcpp::CharacterVector out = Rcpp::wrap(_elements);
	out.attr("dim") = Rcpp::Dimension(_dim);
	return out;
}
*/

}

#endif
