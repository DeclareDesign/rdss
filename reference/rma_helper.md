# Helper function for rma function in metafor package

See https://book.declaredesign.org/complex-designs.html#meta-analysis

## Usage

``` r
rma_helper(data, yi, sei, method = "REML", ...)
```

## Arguments

- data:

  A data frame with one row per study.

- yi:

  The bare column name of the study estimates.

- sei:

  The bare column name of their standard errors.

- method:

  character string to specify whether a fixed- or a random/mixed-effects
  model should be fitted. A fixed-effects model (with or without
  moderators) is fitted when using method = "FE". Random/mixed-effects
  models are fitted by setting method equal to one of the following:
  "DL", "HE", "SJ", "ML", "REML", "EB", "HS", "HSk", or "GENQ". Default
  is "REML".

- ...:

  Further arguments to
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

## Value

An `rma.uni` fit; pass it to
[`rma_mu_tau()`](https://declaredesign.org/r/rdss/reference/rma_mu_tau.md)
to tidy it.

## Details

Fits
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
to study-level estimates and standard errors. If the fit fails, it
returns the error as an object
[`rma_mu_tau()`](https://declaredesign.org/r/rdss/reference/rma_mu_tau.md)
recognizes, so one failed simulation does not stop a diagnosis.

## Examples

``` r

set.seed(343)
studies <- data.frame(
  est = rnorm(10, mean = 0.2, sd = 0.1),
  se = runif(10, min = 0.05, max = 0.15)
)
fit <- rma_helper(studies, yi = est, sei = se)
fit
#> 
#> Random-Effects Model (k = 10; tau^2 estimator: REML)
#> 
#> tau^2 (estimated amount of total heterogeneity): 0.0013 (SE = 0.0035)
#> tau (square root of estimated tau^2 value):      0.0367
#> I^2 (total heterogeneity / total variability):   17.27%
#> H^2 (total variability / sampling variability):  1.21
#> 
#> Test for Heterogeneity:
#> Q(df = 9) = 9.7435, p-val = 0.3716
#> 
#> Model Results:
#> 
#> estimate      se    zval    pval   ci.lb   ci.ub      
#>   0.1384  0.0281  4.9166  <.0001  0.0832  0.1936  *** 
#> 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
