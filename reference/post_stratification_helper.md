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

## Examples

``` r

set.seed(343)
# Post-stratification cells: high school graduates and non-graduates in ten
# states, weighted by each state's share of graduates
cells <- expand.grid(state = state.name[1:10], HS = 0:1)
prob_HS <- rep(seq(0.5, 0.9, length.out = 10), 2)
cells$PS_weight <- ifelse(cells$HS == 1, prob_HS, 1 - prob_HS)

survey <- cells[sample(nrow(cells), 1000, replace = TRUE), ]
survey$Y <- rbinom(1000, 1, prob = plogis(0.5 * survey$HS))

model_fit <- lme4::glmer(Y ~ HS + (1 | state), data = survey, family = binomial)
post_stratification_helper(model_fit, data = cells, group = state, weights = PS_weight)
#> # A tibble: 10 × 2
#>    state       estimate
#>    <fct>          <dbl>
#>  1 Alabama        0.560
#>  2 Alaska         0.565
#>  3 Arizona        0.569
#>  4 Arkansas       0.574
#>  5 California     0.578
#>  6 Colorado       0.583
#>  7 Connecticut    0.587
#>  8 Delaware       0.592
#>  9 Florida        0.596
#> 10 Georgia        0.601
```
