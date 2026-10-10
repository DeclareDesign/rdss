# Tidy results from a stanreg regression (deprecated)

Deprecated: use
[`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html),
which this function now calls with the same arguments.

## Usage

``` r
tidy_stan(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...)
```

## Arguments

- x:

  A fit from
  [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html).

- conf.int:

  Logical indicating whether or not to include a confidence interval in
  the tidied output. Defaults to FALSE.

- conf.level:

  The confidence level to use for the confidence interval if conf.int =
  TRUE. Must be strictly greater than 0 and less than 1. Defaults to
  0.95, which corresponds to a 95 percent confidence interval.

- exponentiate:

  Logical indicating whether or not to exponentiate the coefficient
  estimates. Defaults to FALSE.

- ...:

  Further arguments to
  [`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html).

## Value

A data frame with one row per coefficient.

## Details

See
https://book.declaredesign.org/choosing-an-answer-strategy.html#bayesian-formalizations

## Examples

``` r

# \donttest{
fit <- rstanarm::stan_glm(mpg ~ wt, data = mtcars, chains = 1, iter = 1000,
                          refresh = 0, seed = 343)

# Prefer broom.mixed::tidy(fit, conf.int = TRUE)
tidy_stan(fit, conf.int = TRUE)
#> This function is deprecated. Please use the 'tidy' function from the 'broom.mixed' package.
#> # A tibble: 2 × 5
#>   term        estimate std.error conf.low conf.high
#>   <chr>          <dbl>     <dbl>    <dbl>     <dbl>
#> 1 (Intercept)    37.3      1.88     33.2      41.0 
#> 2 wt             -5.31     0.566    -6.45     -4.18
# }
```
