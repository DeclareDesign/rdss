# Round and pad a number to a specific decimal place

Round and pad a number to a specific decimal place

## Usage

``` r
format_num(x, digits = 3)
```

## Arguments

- x:

  Numeric vector

- digits:

  Number of digits to retain

## Value

a character vector of formatted numbers

## Examples

``` r

std.error <- c(0.12, 0.001, 1.2)
format_num(std.error)
#> [1] "0.120" "0.001" "1.200"
```
