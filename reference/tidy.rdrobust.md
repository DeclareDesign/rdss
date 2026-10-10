# Tidy helper function for rdrobust function

A [`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
method for fits from
[`rdrobust::rdrobust()`](https://rdrr.io/pkg/rdrobust/man/rdrobust.html),
so they can be used in
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.html).
Use
[`rdrobust_helper()`](https://declaredesign.org/r/rdss/reference/rdrobust_helper.md)
to fit the model.

## Usage

``` r
# S3 method for class 'rdrobust'
tidy(x, ...)
```

## Arguments

- x:

  A fit from
  [`rdrobust::rdrobust()`](https://rdrr.io/pkg/rdrobust/man/rdrobust.html).

- ...:

  Not used.

## Value

A data frame with one row per estimate rdrobust reports (conventional,
bias-corrected, and robust) and columns `term`, `estimate`, `std.error`,
`statistic`, `p.value`, `conf.low`, `conf.high`, and `cutoff`.

## Details

See
https://book.declaredesign.org/observational-causal.html#regression-discontinuity-designs
