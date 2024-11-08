When adding or modifying a function, make sure the following have been updated:

- Errors should be handled.
	a. Set status codes to report any conditions the user might need to know
	   about.
	b. Consider adding an `error_action` to control whether some statuses
	   should be thrown as errors, raised as warnings, or printed.

- Function should have appropriate args struct, if needed.
	a. Should contain reasonable defaults.
	b. Should have a conversion operator to an Rcpp List.
	c. Should have a constructor that takes an Rcpp List.

- Function should have an appropriate results struct, if needed.
	a. Should have a conversion operator to an Rcpp List.

- The header file should be included in `inst/include/fntl.h` so that
  function(s) can be called by external code.

- Rcpp interface functions should be present in `src/rcpp-interface.h` and
  `src/rcpp-interface.cpp`. Make sure Roxygen there is updated.

- Function should have appropriate sections in the vignette. This should
  include at least one example in `vignettes/examples`

- Add tests to `inst/tinytest`.

