# Find the covariate that best predicts treatment effect heterogeneity

An inquiry handler. For each covariate, regresses the true unit-level
effect `tau` on the covariate cut into intervals, and reports which
covariate gives the highest R-squared. Pair it with
[`causal_forest_handler()`](https://declaredesign.org/r/rdss/reference/causal_forest_handler.md),
whose `var_imp` column is the estimate of the same quantity.

## Usage

``` r
best_predictor(data, covariate_names, cuts = 20)
```

## Arguments

- data:

  A data frame with a column `tau` holding each unit's treatment effect,
  and the covariates named in `covariate_names`.

- covariate_names:

  A character vector of covariates to assess.

- cuts:

  Either a numeric vector of two or more unique cut points or a single
  number (greater than or equal to 2) giving the number of intervals
  into which each covariate is to be cut. Defaults to 20.

## Value

A data frame with one row: `inquiry` (`"best_predictor"`) and
`estimand`, the position in `covariate_names` of the best predictor.

## Details

See
https://book.declaredesign.org/complex-designs.html#discovery-using-causal-forests

## Examples

``` r

set.seed(343)
dat <- data.frame(A = rnorm(500), B = rnorm(500))
dat$tau <- 1 + dat$A

# The effect varies with A, so A (position 1) is the best predictor
best_predictor(dat, covariate_names = c("A", "B"), cuts = 4)
#>          inquiry estimand
#> 1 best_predictor        1
```
