# Round and pad a number to a specific decimal place

Rounds to `digits` decimal places and keeps trailing zeros, so a column
of numbers lines up in a table.

## Usage

``` r
format_num(x, digits = 3)
```

## Arguments

- x:

  A numeric vector.

- digits:

  The number of decimal places. Defaults to 3.

## Value

A character vector.

## Examples

``` r

std.error <- c(0.12, 0.001, 1.2)
format_num(std.error)
#> [1] "0.120" "0.001" "1.200"
```
