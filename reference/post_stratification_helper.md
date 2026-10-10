# Post stratification estimator helper

Predicts each row's outcome from a fitted model, then averages the
predictions within each group using the post-stratification weights. In
the book, the rows are demographic cells and the groups are states.

## Usage

``` r
post_stratification_helper(model_fit, data, group, weights)
```

## Arguments

- model_fit:

  A fitted model that
  [`marginaleffects::predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)
  accepts, e.g. from
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html). Random
  effects are left out of the predictions (`re.form = NA`).

- data:

  A data frame of post-stratification cells to predict for.

- group:

  The bare column name of the grouping variable.

- weights:

  The bare column name of the post-stratification weights, e.g. each
  cell's population share.

## Value

A data frame with one row per group and columns for the group and
`estimate`.

## Details

See
https://book.declaredesign.org/observational-descriptive.html#multi-level-regression-and-poststratification
