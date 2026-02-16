# Helper function for rma function in metafor package

See https://book.declaredesign.org/complex-designs.html#meta-analysis

## Usage

``` r
rma_helper(data, yi, sei, method = "REML", ...)
```

## Arguments

- data:

  a data.frame

- yi:

  unquoted variable name of estimates used in meta-analysis

- sei:

  unquoted variable name of standard errors used in meta-analysis

- method:

  character string to specify whether a fixed- or a random/mixed-effects
  model should be fitted. A fixed-effects model (with or without
  moderators) is fitted when using method = "FE". Random/mixed-effects
  models are fitted by setting method equal to one of the following:
  "DL", "HE", "SJ", "ML", "REML", "EB", "HS", "HSk", or "GENQ". Default
  is "REML".

- ...:

  Further options to be passed to rma

## Value

a data.frame of estimates

## Details

See ?rma for further details
