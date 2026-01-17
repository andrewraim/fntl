#ifndef FNTL_CSC_MAT_BUILDER_H
#define FNTL_CSC_MAT_BUILDER_H

#include <Rcpp.h>
#include "csc-mat.h"
#include "typedefs.h"

namespace fntl {

// Order coordinates (row,col) in column-major order
struct csc_comparator {
	bool operator()(const coord2_t& a, const coord2_t& b) const
	{
		if (a[1] == b[1]) {
			return a[0] < b[0];
		}
		return a[1] < b[1];
	}
};

template <typename T>
class csc_mat_builder
{
public:
	csc_mat_builder(unsigned int m, unsigned int n);
	void set(unsigned int i, unsigned int j, const T& x);
	csc_mat<T> get() const;

private:
	unsigned int _m;
	unsigned int _n;
	std::map<coord2_t, T, csc_comparator> _elements;
};

template <typename T>
csc_mat_builder<T>::csc_mat_builder(unsigned int m, unsigned int n)
: _m(m), _n(n), _elements()
{
}

template <typename T>
void csc_mat_builder<T>::set(unsigned int i, unsigned int j, const T& x)
{
	if (i >= _m || j >= _n) {
		Rcpp::stop("Index out of bounds");
	}

	coord2_t idx = {i, j};
	_elements[idx] = x;
}

template <typename T>
csc_mat<T> csc_mat_builder<T>::get() const
{
	unsigned int N_bdd = _m * _n;
	csc_mat<T> out(_m, _n);
	out.p.assign(_n + 1, N_bdd);

	auto itr = _elements.begin();
	for (; itr != _elements.end(); ++itr) {
		const coord2_t& idx = itr->first;
		const T& v = itr->second;

		unsigned int i = idx[0];
		unsigned int j = idx[1];

		if (out.p[j] == N_bdd) {
			out.p[j] = out.x.size();
		}
		out.i.push_back(i);
		out.x.push_back(v);
	}

	out.p[_n] = out.x.size();
	for (int j = _n-1; j >= 0; j--) {
		out.p[j] = std::min(out.p[j], out.p[j+1]);
	}

	return out;
}

}

#endif

