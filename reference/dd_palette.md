# Color palettes used in the book Research Design in the Social Sciences

Based on Karthik Ram's wesanderson package
(https://github.com/karthik/wesanderson)

## Usage

``` r
dd_palette(name, n)
```

## Arguments

- name:

  The palette name, as a string. One of the names listed below.

- n:

  The number of colors to return, from the start of the palette.
  Defaults to all of them.

## Value

A character vector of hex color codes.

## Details

Palettes:

- `three_color_palette`: light blue, orange, pink

- `grey_palette`: light blue, orange, pink, light gray

- `quilt_palette`: light gray, pink, purple, light blue, orange

- `two_color_palette`: dark blue, pink

- `quilt_three_color_palette`: light gray, translucent light blue, light
  blue

- `two_color_gray`: dark blue, light gray

Single colors: `dd_dark_blue` (`"#3564ED"`), `dd_light_blue`
(`"#72B4F3"`), `dd_orange` (`"#F38672"`), `dd_purple` (`"#7E43B6"`),
`dd_gray` (`gray(0.2)`), `dd_pink` (`"#C6227F"`), `dd_light_gray`
(`gray(0.8)`), and the translucent `dd_dark_blue_alpha` and
`dd_light_blue_alpha`.

## Examples

``` r
dd_palette("three_color_palette")
#> [1] "#72B4F3" "#F38672" "#C6227F"
dd_palette("quilt_palette", n = 3)
#> [1] "#CCCCCC" "#C6227F" "#7E43B6"
```
