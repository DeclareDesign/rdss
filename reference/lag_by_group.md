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
