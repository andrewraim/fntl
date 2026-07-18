// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
void logger_ex()
{
	fntl::logger("About to do something important\n");
	Rprintf("...\n");
	fntl::logger("Finished doing something important\n");
}
