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
