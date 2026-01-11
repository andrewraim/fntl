#ifndef FNTL_WHICH_H
#define FNTL_WHICH_H

#include <Rcpp.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

namespace fntl {

template <typename T, int RTYPE>
Rcpp::IntegerMatrix which(const Rcpp::Matrix<RTYPE>& X,
	const std::function<bool(T)>& f, bool one_based = false)
{
	unsigned int m = X.nrow();
	unsigned int n = X.ncol();
	std::vector<unsigned int> idx_row;
	std::vector<unsigned int> idx_col;

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int i = 0; i < m; i++) {
			bool ind = f(X(i,j));
			if (ind) {
				idx_row.push_back(i);
				idx_col.push_back(j);
			}
		}
	}

	unsigned int k = idx_row.size();
	Rcpp::IntegerMatrix out(k, 2);
	for (unsigned int i = 0; i < k; i++) {
		out(i, 0) = idx_row[i] + one_based;
		out(i, 1) = idx_col[i] + one_based;
	}

	colnames(out) = Rcpp::CharacterVector::create("row", "col");
	return out;
}

template <typename T>
Rcpp::IntegerMatrix which(const csc_mat<T>& X,
	const std::function<bool(T)>& f, bool one_based = false)
{
	unsigned int m = X.m;
	unsigned int n = X.n;
	std::vector<unsigned int> idx_row;
	std::vector<unsigned int> idx_col;

	for (unsigned int j = 0; j < n; j++) {
		for (unsigned int l = X.p[j]; l < X.p[j+1]; l++) {
			unsigned int i = X.i[l];
			bool ind = f(X(i,j));
			if (ind) {
				idx_row.push_back(i);
				idx_col.push_back(j);
			}
		}
	}

	unsigned int k = idx_row.size();
	Rcpp::IntegerMatrix out(k, 2);
	for (unsigned int i = 0; i < k; i++) {
		out(i, 0) = idx_row[i] + one_based;
		out(i, 1) = idx_col[i] + one_based;
	}

	colnames(out) = Rcpp::CharacterVector::create("row", "col");
	return out;
}

}

#endif
