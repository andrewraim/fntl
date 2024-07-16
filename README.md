# Introduction

`fntl` is an R package to facilitate Rcpp programming. It provides a C++ API
for routinely used numerical tools such as integration, root-finding, and
optimization, where function arguments are given as lambdas. This enables the
development of R-like code in C++ where functions can be defined on the fly
and use variables in the surrounding environment.

See the included vignette for a more in-depth discussion of the package and an
API guide.

# Installation

The `fntl` package may be installed directly from Github using a standard R
command like the following.

```r
devtools::install_github("andrewraim/fntl", ref = "v0.1.0")
```

Here, `v0.1.0` represents a tagged release; replace it with a later version if
one exists.


# Getting Started

The following example computes the integral

$$
B(a,b) = \int_0^1 x^{a-1} (1-x)^{b-1} dx.
$$

Create the file `example.cpp` with the following contents.

```cpp
// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List example(double a, double b)
{
    fntl::dfd f = [&](double x) {
        return std::pow(x, a - 1) * std::pow(1 - x, b - 1);
    };
    auto out = fntl::integrate(f, 0, 1);
    return Rcpp::wrap(out);
}
```

The `example` function may be called through R as follows.

```r
Rcpp::sourceCpp("example.cpp")
example(2, 3)
```
