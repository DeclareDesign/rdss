# Add alpha transparency to a color defined in hexadecimal

Add alpha transparency to a color defined in hexadecimal

## Usage

``` r
hex_add_alpha(col, alpha)
```

## Arguments

- col:

  A color as a six-digit hex code, e.g. `"#72B4F3"`.

- alpha:

  Opacity, from 0 (transparent) to 1 (opaque).

## Value

The color as an eight-digit hex code.

## Examples

``` r
hex_add_alpha("#72B4F3", 0.5)
#> [1] "#72B4F380"
```
