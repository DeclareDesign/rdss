# Extract mu and tau-squared from a random-effects meta-analysis

A tidier for fits from
[`rma_helper()`](https://declaredesign.org/r/rdss/reference/rma_helper.md):
`mu` is the average effect across studies and `tau_sq` the variance of
the true effects across studies.

## Usage

``` r
rma_mu_tau(fit)
```

## Arguments

- fit:

  A fit from
  [`rma_helper()`](https://declaredesign.org/r/rdss/reference/rma_helper.md)
  or
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

## Value

A data frame with two rows, `mu` and `tau_sq`, with estimates, standard
errors, and confidence intervals (for `tau_sq`, intervals only when
`method = "REML"`). If the fit failed, one row with `estimate = NA` and
`error = TRUE`.

## Details

See https://book.declaredesign.org/complex-designs.html#meta-analysis

## Examples

``` r

set.seed(343)
studies <- data.frame(
  est = rnorm(10, mean = 0.2, sd = 0.1),
  se = runif(10, min = 0.05, max = 0.15)
)
fit <- rma_helper(studies, yi = est, sei = se)
rma_mu_tau(fit)
#> # A tibble: 2 × 7
#>   term   estimate std.error statistic      p.value conf.low conf.high
#>   <chr>     <dbl>     <dbl>     <dbl>        <dbl>    <dbl>     <dbl>
#> 1 mu      0.138     0.0281       4.92  0.000000880   0.0832    0.194 
#> 2 tau_sq  0.00135   0.00350     NA    NA             0         0.0166
```
