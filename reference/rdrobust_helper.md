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
