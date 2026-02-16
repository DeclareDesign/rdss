# Generate lags in grouped data

See
https://book.declaredesign.org/observational-causal.html#difference-in-differences

## Usage

``` r
lag_by_group(x, groups, n = 1, order_by, default = NA)
```

## Arguments

- x:

  Vector of values

- groups:

  Grouping variable

- n:

  Positive integer of length 1, giving the number of positions to lead
  or lag by

- order_by:

  Ordering variable withing group (e.g., time)

- default:

  Value used for non-existent rows. Defaults to NA.

## Value

vector of lagged values
