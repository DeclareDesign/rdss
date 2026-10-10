# Tidy helper function for causal_forest function

Splits the data at random into a training and a test set, fits
[`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
on the training set, and predicts each unit's treatment effect. The data
must contain the outcome as `Y` and the treatment as `Z`.

## Usage

``` r
causal_forest_handler(data, covariate_names, share_train = 0.5, ...)
```

## Arguments

- data:

  A data frame with columns `Y`, `Z`, and the covariates named in
  `covariate_names`.

- covariate_names:

  A character vector naming the covariate columns the forest may split
  on.

- share_train:

  The share of units assigned to the training set. Defaults to 0.5.

- ...:

  Further arguments passed to
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html).

## Value

`data` with four columns added: `pred` (the predicted treatment effect),
`var_imp` (the position in `covariate_names` of the most important
covariate), and the logical indicators `train` and `test`.

## Details

See
https://book.declaredesign.org/complex-designs.html#discovery-using-causal-forests

## Examples

``` r

library(DeclareDesign)
#> Loading required package: randomizr
#> Loading required package: fabricatr
#> Loading required package: estimatr
library(ggplot2)
#> 
#> Attaching package: ‘ggplot2’
#> The following object is masked from ‘package:DeclareDesign’:
#> 
#>     vars

dat <- fabricate(
   N = 1000,
   A = rnorm(N),
   B = rnorm(N),
   Z = complete_rs(N),
   Y = A*Z + rnorm(N))

# note: remove num.threads = 1 to use more processors
estimates <- causal_forest_handler(data = dat, covariate_names = c("A", "B"), num.threads = 1)

ggplot(data = estimates, aes(A, pred)) + geom_point()
```
