# rdss: Helper functions for Research Design in the Social Sciences book

[![CRAN
status](https://www.r-pkg.org/badges/version/rdss)](https://cran.r-project.org/package=rdss)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rdss?color=green)](https://r-pkg.org/pkg/rdss)
[![Build
status](https://github.com/DeclareDesign/rdss/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DeclareDesign/rdss/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/DeclareDesign/rdss/graph/badge.svg)](https://app.codecov.io/gh/DeclareDesign/rdss)

Helper functions for using the book [*Research Design in the Social
Sciences: Declaration, Diagnosis,
Redesign*](https://book.declaredesign.org/) by Blair, Coppock, and
Humphreys 2023 (Princeton University Press).

## Installation

Install the released version from CRAN:

``` r

install.packages("rdss")
```

or the development version from GitHub:

``` r

remotes::install_github("DeclareDesign/rdss")
```

[`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md),
used in the book’s section on experiments over networks, also needs the
‘interference’ package, which is not on CRAN:

``` r

remotes::install_github("szonszein/interference")
```

[`did_multiplegt_tidy()`](https://declaredesign.org/r/rdss/reference/did_multiplegt_tidy.md),
used in the book’s section on difference-in-differences, needs the
‘polars’ package, which is also not on CRAN:

``` r

install.packages("polars", repos = "https://rpolars.r-universe.dev")
```

## Example

Each helper fills one step of a declaration in the book, and its help
page links to the section that uses it.
[`lag_by_group()`](https://declaredesign.org/r/rdss/reference/lag_by_group.md),
for instance, builds the lagged treatment indicator in the
difference-in-differences design:

``` r

library(rdss)
library(dplyr)

panel <-
  tibble(
    unit = rep(c("a", "b"), each = 3),
    period = rep(1:3, times = 2),
    D = c(0, 1, 1, 0, 0, 1)
  ) |>
  mutate(D_lag = lag_by_group(D, groups = unit, n = 1, order_by = period))

panel
```

``` R
## # A tibble: 6 × 4
##   unit  period     D D_lag
##   <chr>  <int> <dbl> <dbl>
## 1 a          1     0    NA
## 2 a          2     1     0
## 3 a          3     1     1
## 4 b          1     0    NA
## 5 b          2     0     0
## 6 b          3     1     0
```
