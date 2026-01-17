#ifndef FNTL_CSR_MAT_BUILDER_H
#define FNTL_CSR_MAT_BUILDER_H

#include <Rcpp.h>
#include "csr-mat.h"
#include "typedefs.h"

namespace fntl {

// Order coordinates (row,col) in row-major order
struct csr_comparator {
	bool operator()(const coord2_t& a, const coord2_t& b) const
	{
		if (a[0] == b[0]) {
			return a[1] < b[1];
		}
		return a[0] < b[0];
	}
};

template <typename T>
class csr_mat_builder
{
public:
	csr_mat_builder(unsigned int m, unsigned int n);
	void set(unsigned int i, unsigned int j, const T& x);
	csr_mat<T> get() const;

private:
	unsigned int _m;
	unsigned int _n;
	std::map<coord2_t, T, csr_comparator> _elements;
};

template <typename T>
csr_mat_builder<T>::csr_mat_builder(unsigned int m, unsigned int n)
: _m(m), _n(n), _elements()
{
}

template <typename T>
void csr_mat_builder<T>::set(unsigned int i, unsigned int j, const T& x)
{
	if (i >= _m || j >= _n) {
		Rcpp::stop("Index out of bounds");
	}

	coord2_t idx(i, j);
	_elements[idx] = x;
}

template <typename T>
csr_mat<T> csr_mat_builder<T>::get() const
{
	unsigned int N_bdd = _m * _n;
	csr_mat<T> out(_m, _n);
	out.p.assign(_m + 1, N_bdd);

	auto itr = _elements.begin();
	for (; itr != _elements.end(); ++itr) {
		const coord2_t& idx = itr->first;
		const T& v = itr->second;

		unsigned int i = idx[0];
		unsigned int j = idx[1];

		if (out.p[i] == N_bdd) {
			out.p[i] = out.x.size();
		}
		out.i.push_back(i);
		out.x.push_back(v);
	}

	out.p[_m] = out.x.size();
	for (int i = _m-1; i >= 0; i--) {
		out.p[i] = std::min(out.p[i], out.p[i+1]);
	}

	return out;
}

}

#endif

