# ggplot theme used in the book Research Design in the Social Sciences

[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with light gray major gridlines only, no axis ticks, and no legend. The
book labels series directly, so it hides the legend; add
`theme(legend.position = "right")` after it to get one back.

## Usage

``` r
theme_dd()
```

## Value

A ggplot2 theme.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_dd()

```
