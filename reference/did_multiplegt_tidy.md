# Tidy helper function for did_multiplegt

Runs
[`DIDmultiplegt::did_multiplegt()`](https://rdrr.io/pkg/DIDmultiplegt/man/did_multiplegt.html),
the de Chaisemartin and D'Haultfoeuille estimator, and returns its
estimates as a data frame.

## Usage

``` r
did_multiplegt_tidy(data, ...)
```

## Arguments

- data:

  A data frame, passed to `did_multiplegt()` as `df`.

- ...:

  Further arguments to `did_multiplegt()`. These must include `mode`
  and, for `mode = "dyn"`, the column names of the outcome (`outcome`),
  group (`group`), time period (`time`), and treatment (`treatment`), as
  strings, and the number of effects to estimate (`effects`).
  `graph_off` defaults to `TRUE`, so that no plot is drawn.

## Value

For `mode = "dyn"`, a data frame with one row per effect and columns
`term` (`"Effect_1"`, `"Effect_2"`, ...), `estimate`, `std.error`,
`conf.low`, `conf.high`, `n` (the number of group-period cells used),
and `n_switchers` (the number of switching groups). For `mode = "old"`,
a data frame with one column, `estimate`.

## Details

Use `mode = "dyn"`. It estimates the effect `l` periods after a group
first switches treatment, for `l` from 1 to `effects`; `effects = 1` is
the effect in the period of the switch. This mode runs
`did_multiplegt_dyn()` from the 'DIDmultiplegtDYN' package, which also
needs the 'polars' package. polars is not on CRAN; install it with
`install.packages("polars", repos = "https://rpolars.r-universe.dev")`
and attach it with
[`library(polars)`](https://pola-rs.github.io/r-polars/).

`mode = "old"` is still passed through, but in 'DIDmultiplegt' 2.1.0 it
returns `NaN`: it takes first differences with
[`stats::lag()`](https://rdrr.io/r/stats/lag.html), which does not shift
the data, so no group is ever seen to switch. The function warns when
that happens.

See
https://book.declaredesign.org/observational-causal.html#difference-in-differences

## Examples

``` r

library(polars)

set.seed(343)
# 20 units in 10 periods; units switch into treatment at different times
panel <- data.frame(unit = rep(1:20, times = 10), period = rep(1:10, each = 20))
panel$D <- as.numeric(panel$period > (panel$unit %% 5) + 4)
panel$Y <- 0.5 * panel$D + rnorm(nrow(panel))

did_multiplegt_tidy(
  panel, mode = "dyn",
  outcome = "Y", group = "unit", time = "period", treatment = "D",
  effects = 2
)
#> # A tibble: 2 × 7
#>   term           estimate std.error conf.low conf.high     n n_switchers
#>   <chr>             <dbl>     <dbl>    <dbl>     <dbl> <dbl>       <dbl>
#> 1 "Effect_1    "    0.583     0.458   -0.314      1.48    56          16
#> 2 "Effect_2    "    0.565     0.433   -0.283      1.41    36          12
```
