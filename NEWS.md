# fntl 0.1.3

- Use `static_cast` rather than `to_underlying` to cast enums to integers.
- Use lowercase variable and function names with underscores rather than camel
  case.
- Add support in API to update an existing proposal. This can be used to tune
  proposals within a Gibbs sampler. A detailed example has been added to the
  vignette.

# fntl 0.1.2

- Added a `NEWS.md` file to track changes to the package.
- Corrections to handling of `fnscale` in univariate optimization.
- Add functions to support truncated distributions.
