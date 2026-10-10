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

## Examples

``` r

set.seed(343)
rd <- data.frame(X = runif(1000, min = -1, max = 1))
rd$Y <- 0.5 * (rd$X > 0) + rd$X + rnorm(1000, sd = 0.5)

fit <- rdrobust_helper(rd, y = Y, x = X, c = 0)
tidy(fit)
#>             term  estimate std.error statistic      p.value  conf.low conf.high
#> 1   Conventional 0.4394154 0.1193589  3.681462 0.0002319002 0.2054762 0.6733547
#> 2 Bias-Corrected 0.4526316 0.1193589  3.792188 0.0001493258 0.2186923 0.6865708
#> 3         Robust 0.4526316 0.1458573  3.103249 0.0019140878 0.1667564 0.7385067
```
