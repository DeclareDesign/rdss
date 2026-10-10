# Conjoint experiment measurement handler: records which profile in each task is chosen

Applies `utility_fn` and, within each subject and task, compares the two
profiles and sets `choice` to 1 for the one with the higher utility `U`
(the second profile on a tie).

## Usage

``` r
conjoint_measurement(data, utility_fn)
```

## Arguments

- data:

  A data frame with one row per profile and columns `subject`, `task`,
  and `profile`. Each task must hold exactly two profiles.

- utility_fn:

  A function that takes `data` and returns it with a column `U` added,
  the utility the subject gets from each profile.

## Value

`data` with `U` and `choice` added.

## Details

See
https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments

## Examples

``` r

set.seed(343)
levels_list <- list(gender = c("woman", "man"), age = c("young", "old"))

# 25 subjects, each choosing between two profiles in each of four tasks
profiles <- expand.grid(profile = 1:2, task = 1:4, subject = 1:25)
profiles <- conjoint_assignment(profiles, levels_list)

utility_fn <- function(data) {
  data$U <- 0.5 * (data$gender == "woman") - 0.2 * (data$age == "old") +
    rnorm(nrow(data))
  data
}

head(conjoint_measurement(profiles, utility_fn))
#> # A tibble: 6 × 7
#>   profile  task subject gender age        U choice
#>     <int> <int>   <int> <fct>  <fct>  <dbl>  <dbl>
#> 1       1     1       1 man    old    0.448      0
#> 2       2     1       1 woman  old    0.956      1
#> 3       1     2       1 man    young -0.182      0
#> 4       2     2       1 man    old    0.176      1
#> 5       1     3       1 woman  young  0.228      1
#> 6       2     3       1 woman  old   -0.148      0
```
