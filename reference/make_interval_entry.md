# Format confidence intervals for nice printing

Format confidence intervals for nice printing

## Usage

``` r
make_interval_entry(conf.low, conf.high, digits = 2)
```

## Arguments

- conf.low:

  a numeric vector of lower bounds

- conf.high:

  a numeric vector of upper bounds

- digits:

  number of digits to retain

## Value

a character vector of intervals

## Examples

``` r
conf.low <- c(-0.1652, 0.00304, -6.352)
conf.high <- c(0.3052, 0.00696, -1.648)

make_interval_entry(conf.low, conf.high)
#> [1] "[-0.17, 0.31]"  "[0.00, 0.01]"   "[-6.35, -1.65]"

```
