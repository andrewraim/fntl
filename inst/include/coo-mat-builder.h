
#ifndef FNTL_COO_MAT_BUILDER_H
#define FNTL_COO_MAT_BUILDER_H

#include <Rcpp.h>
#include "coo-mat.h"
#include "typedefs.h"

namespace fntl {

// Order coordinates (row,col) in column-major order
struct coo_comparator {
	bool operator()(const coord2_t& a, const coord2_t& b) const
	{
		if (a[1] == b[1]) {
			return a[0] < b[0];
		}
		return a[1] < b[1];
	}
};

template <typename T>
class coo_mat_builder
{
public:
	coo_mat_builder(unsigned int m, unsigned int n);
	void set(unsigned int i, unsigned int j, const T& x);
	coo_mat<T> get() const;

private:
	unsigned int _m;
	unsigned int _n;
	std::map<coord2_t, T, coo_comparator> _elements;
};

template <typename T>
coo_mat_builder<T>::coo_mat_builder(unsigned int m, unsigned int n)
: _m(m), _n(n), _elements()
{
}

template <typename T>
void coo_mat_builder<T>::set(unsigned int i, unsigned int j, const T& x)
{
	if (i >= _m || j >= _n) {
		Rcpp::stop("Index out of bounds");
	}

	coord2_t idx(i, j);
	_elements[idx] = x;
}

template <typename T>
coo_mat<T> coo_mat_builder<T>::get() const
{
	coo_mat<T> out(_m, _n);

	auto itr = _elements.begin();
	for (; itr != _elements.end(); ++itr) {
		const coord2_t& idx = itr->first;
		const T& v = itr->second;

		unsigned int i = idx[0];
		unsigned int j = idx[1];

		out.i.push_back(i);
		out.j.push_back(j);
		out.x.push_back(v);
	}

	return out;
}

}

#endif

