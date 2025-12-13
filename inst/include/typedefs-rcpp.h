#ifndef FNTL_TYPEDEFS_RCPP_H
#define FNTL_TYPEDEFS_RCPP_H

/*
* typedefs involving Rcpp types are kept separate from other typedefs. This is
* to facilitate the forward declarations needed in serialization.h.
*/

#include <Rcpp.h>

namespace fntl {

typedef std::function<double(const Rcpp::NumericVector&)> dfv;
typedef std::function<double(const Rcpp::NumericVector&, const Rcpp::NumericVector&)> dfvv;
typedef std::function<bool(const Rcpp::NumericVector&, const Rcpp::NumericVector&)> bfvv;
typedef std::function<Rcpp::NumericVector(const Rcpp::NumericVector&)> vfv;
typedef std::function<Rcpp::NumericMatrix(const Rcpp::NumericVector&)> mfv;

}

#endif
