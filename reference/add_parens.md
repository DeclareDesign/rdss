# Add parentheses around standard error estimates

Add parentheses around standard error estimates

## Usage

``` r
add_parens(x, digits = 3)
```

## Arguments

- x:

  Numeric vector

- digits:

  Number of digits to retain

## Value

A character vector with enclosing parentheses

## Examples

``` r

std.error <- c(0.12, 0.001, 1.2)
add_parens(std.error)
#> [1] "(0.120)" "(0.001)" "(1.200)"
```
