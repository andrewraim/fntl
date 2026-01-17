// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::IntegerVector temp_ex(const Rcpp::IntegerVector& xx)
{
	fntl::mdarray<int, 3> x(xx);
	// fntl::mdarray<int, 3> x({ 2, 2, 3 });

	fntl::coord_t<3> dims = x.dim();

	for (unsigned int k = 0; k < dims[2]; k++) {
		for (unsigned int j = 0; j < dims[1]; j++) {
			for (unsigned int i = 0; i < dims[0]; i++) {
				x({i, j, k}) *= 10;
			}
		}
	}

	return x.to_Vector<INTSXP>();
}
