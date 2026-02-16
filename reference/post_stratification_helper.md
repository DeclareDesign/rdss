# Post stratification estimator helper

Calculates predicted values from a multilevel regression and the
post-stratified state-level estimates

## Usage

``` r
post_stratification_helper(model_fit, data, group, weights)
```

## Arguments

- model_fit:

  a model fit object from, e.g., glmer or lm_robust

- data:

  a data.frame

- group:

  unquoted name of the group variable to construct estimates for

- weights:

  unquoted name of post-stratification weights variable

## Value

data.frame of post-stratified group-level estimates

## Details

Please see
https://book.declaredesign.org/observational-descriptive.html#multi-level-regression-and-poststratification
