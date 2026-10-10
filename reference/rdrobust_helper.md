# Helper function for using rdrobust as a model in `declare_estimator`

Lets
[`rdrobust::rdrobust()`](https://rdrr.io/pkg/rdrobust/man/rdrobust.html),
which takes vectors, be called with a data frame and column names. Pass
it as `.method` in
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.html);
[`tidy.rdrobust()`](https://declaredesign.org/r/rdss/reference/tidy.rdrobust.md)
then tidies the fit.

## Usage

``` r
rdrobust_helper(data, y, x, subset = NULL, ...)
```

## Arguments

- data:

  A data frame.

- y:

  The bare column name of the outcome.

- x:

  The bare column name of the running variable.

- subset:

  An optional logical expression in terms of the columns of `data`,
  selecting the rows to use.

- ...:

  Further arguments to
  [`rdrobust::rdrobust()`](https://rdrr.io/pkg/rdrobust/man/rdrobust.html),
  e.g. `c` for the cutoff.

## Value

rdrobust model fit object

## Details

See
https://book.declaredesign.org/observational-causal.html#regression-discontinuity-designs
