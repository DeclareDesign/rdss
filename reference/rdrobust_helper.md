# Helper function for using rdrobust as a model in `declare_estimator`

Helper function for using rdrobust as a model in `declare_estimator`

## Usage

``` r
rdrobust_helper(data, y, x, subset = NULL, ...)
```

## Arguments

- data:

  a data.frame

- y:

  unquoted name of the outcome variable

- x:

  unquoted name of the running variable

- subset:

  an optional vector specifying a subset of observations to be used in
  the fitting process

- ...:

  Other arguments to `rdrobust`

## Value

rdrobust model fit object
