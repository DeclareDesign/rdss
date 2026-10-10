# Conjoint experiment inquiries handler

Computes the true average marginal component effect (AMCE) of every
attribute level relative to that attribute's first level. Each AMCE is
the average change in the probability that the second profile in a task
is chosen when the attribute moves from its first level to the given
level, with the other attributes drawn at random by
[`conjoint_assignment()`](https://declaredesign.org/r/rdss/reference/conjoint_assignment.md).

## Usage

``` r
conjoint_inquiries(data, levels_list, utility_fn)
```

## Arguments

- data:

  A data frame with one row per profile and columns `subject`, `task`,
  and `profile` (1 or 2).

- levels_list:

  A named list of attribute levels, as in
  [`conjoint_assignment()`](https://declaredesign.org/r/rdss/reference/conjoint_assignment.md).
  The first level of each attribute is the reference.

- utility_fn:

  A function that adds the utility column `U`, as in
  [`conjoint_measurement()`](https://declaredesign.org/r/rdss/reference/conjoint_measurement.md).

## Value

A data frame with one row per non-reference level: `attribute`,
`reference`, `level`, `inquiry`, and `estimand`.

## Details

See
https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments

## Examples

``` r

set.seed(343)
levels_list <- list(gender = c("woman", "man"), age = c("young", "old"))

# 25 subjects, each choosing between two profiles in each of four tasks
profiles <- expand.grid(profile = 1:2, task = 1:4, subject = 1:25)

utility_fn <- function(data) {
  data$U <- 0.5 * (data$gender == "woman") - 0.2 * (data$age == "old") +
    rnorm(nrow(data))
  data
}

conjoint_inquiries(profiles, levels_list, utility_fn)
#> # A tibble: 2 × 5
#>   attribute reference level inquiry   estimand
#>   <chr>     <chr>     <chr> <chr>        <dbl>
#> 1 gender    woman     man   genderman    -0.02
#> 2 age       young     old   ageold       -0.14
```
