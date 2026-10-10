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
