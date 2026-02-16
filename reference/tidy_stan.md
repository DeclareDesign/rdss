# Tidy results from a stanreg regresion and exponentiate the estimated coefficient

Note no standard errors or other summary statistics are provided

This function is deprecated. Please use the 'tidy' function from the
'broom.mixed' package.

## Usage

``` r
tidy_stan(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...)

tidy_stan(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...)
```

## Arguments

- x:

  A stanreg fit from stan_glm

- conf.int:

  Logical indicating whether or not to include a confidence interval in
  the tidied output. Defaults to FALSE.

- conf.level:

  The confidence level to use for the confidence interval if conf.int =
  TRUE. Must be strictly greater than 0 and less than 1. Defaults to
  0.95, which corresponds to a 95 percent confidence interval.

- exponentiate:

  Logical indicating whether or not to exponentiate the the coefficient
  estimates. Defaults to FALSE.

- ...:

  Other arguments to broom.mixed::tidy

## Value

data.frame of results

data.frame of results

## Details

See
https://book.declaredesign.org/choosing-an-answer-strategy.html#bayesian-formalizations

See
https://book.declaredesign.org/choosing-an-answer-strategy.html#bayesian-formalizations
