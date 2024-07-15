# Preliminary Notes

Functions in R are treated as first-class objects that can be constructed on
the fly based on objects in the environment and passed to other functions. This
is one factor that can make methods much easier to implement in R than in
C/C++, where functions are passed using function pointers or functor objects.
However, performance in an pure C++ implementation can be dramatically faster
than one in R, or one which repeatedly passes objects between C++ and R.
The experience of programming with functions in C++ can be made much closer to
R using STL `std::function` objects: especially used in tandem with lambda
functions. 

The idea for this package is support use of `std::function` with operations
like `optim` and `uniroot` in R, where a function is one of the primary
inputs. The package will provide interfaces of methods such as `optim` and
`uniroot` which `std::function` objects as inputs and handle the underlying
C/C++ calls.

Some design principles for this package are the following.

- Avoid any external dependencies beyond base R, the Rcpp package, and their
  dependencies.
- Where possible, prefer to use C implementations of methods which are exported
  within base R.
- We can roll our own implementation of a method when necessary. We may opt for
  a more basic algorithm or a less optimized code than, say, one in Netlib.
  Good error handling and correctness of the code are most important.

Functions to implement:

- [x] nmmin: Nelder-Mead optimization
- [x] vmmin: BFGS optimization
- [x] cgmin: Conjugate gradient optimization
- [x] lbfgsb: L-BFGS-B optimization
- [ ] samin: Simulated annealing optimization
- [ ] nlm / optif9: Nonlinear minimization using Newton-Raphson
- [ ] One-dimensional optimization (e.g. using Brent's method)
- [x] Golden Section Search optimization
- [x] integrate
- [x] uniroot: root finding on the real line
- [x] Numerical gradient: consider fdridr in Numerical Recipes
- [x] Numerical hessian: call fdhess in R
- [x] Matrix apply functions

Other possibilities:

- constrOptim: linearly constrained optimization
- nlminb: Optimization using PORT routines
- General purpose numerical MLE functions here? Would it add value to have
  something beyond the optimization and Hessian functions?
- A Metropolis sampler. We could take the `logpost` function as an argument.
  Does it belong here?
- Similarly, an implementation of HMC / NUTS. Would it be desirable to use one
  based on numerical differentiation when there are sophisticated ones (e.g.
  Stan) that use auto-differentiation? Also, we probably wouldn't be able to do
  a suite of checks and diagnostics like Stan.


These are not related to functionals, but what about the following? How
granular and specific do we want to make the present package?

- logger
- log-sum-exp, log-add2-exp, and log-sub2-exp
- findInterval: discretize values into bins
- apply: for vectors, matrices, lists, and arrays. I think these can all be
  used with STL functions directly
