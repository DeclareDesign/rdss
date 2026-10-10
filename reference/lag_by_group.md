# Generate lags in grouped data

Lags `x` within each group, ordering by `order_by` rather than by row
order. Write the variables as bare column names inside
[`fabricatr::fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.html)
or
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html);
see the README for an example.

## Usage

``` r
lag_by_group(x, groups, n = 1, order_by, default = NA)
```

## Arguments

- x:

  The variable to lag.

- groups:

  The grouping variable, e.g. a unit identifier.

- n:

  A positive integer: the number of periods to lag by. Defaults to 1.

- order_by:

  Ordering variable within group (e.g., time)

- default:

  The value for rows with no earlier period in their group. Defaults to
  `NA`.

## Value

A vector the length of `x`, in the original row order.

## Details

See
https://book.declaredesign.org/observational-causal.html#difference-in-differences

## Examples

``` r

# Three units observed in four periods; unit u is treated after period u
panel <- expand.grid(unit = 1:3, period = 1:4)
panel$D <- as.numeric(panel$period > panel$unit)

dplyr::mutate(panel, D_lag = lag_by_group(D, groups = unit, order_by = period))
#>    unit period D D_lag
#> 1     1      1 0    NA
#> 2     2      1 0    NA
#> 3     3      1 0    NA
#> 4     1      2 1     0
#> 5     2      2 0     0
#> 6     3      2 0     0
#> 7     1      3 1     1
#> 8     2      3 1     0
#> 9     3      3 0     0
#> 10    1      4 1     1
#> 11    2      4 1     1
#> 12    3      4 1     0
```
