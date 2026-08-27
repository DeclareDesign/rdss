# Format estimates and standard errors for nice printing

Format estimates and standard errors for nice printing

## Usage

``` r
make_se_entry(estimate, std.error, digits = 2)
```

## Arguments

- estimate:

  a numeric vector of parameter estimates

- std.error:

  a numeric vector of standard error estimates

- digits:

  number of digits to retain

## Value

a character vector of formatted estimates and standard errors

## Examples

``` r

estimate <- c(0.07, 0.005, -4)
std.error <- c(0.12, 0.001, 1.2)

make_se_entry(estimate, std.error)
#> [1] "0.07 (0.12)"  "0.01 (0.00)"  "-4.00 (1.20)"
```
