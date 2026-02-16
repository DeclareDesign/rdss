# Best predictor function from causal_forest

Best predictor function from causal_forest

## Usage

``` r
best_predictor(data, covariate_names, cuts = 20)
```

## Arguments

- data:

  A data.frame of covariates

- covariate_names:

  A character vector of covariates to assess

- cuts:

  Either a numeric vector of two or more unique cut points or a single
  number (greater than or equal to 2) giving the number of intervals
  into which each covariate is to be cut.

## Value

a data.frame of the best predictors
