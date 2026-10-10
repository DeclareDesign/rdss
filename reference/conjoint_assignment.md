# Conjoint experiment assignment handler: conducts complete random assignment of all attribute levels

For each attribute, assigns every row one of that attribute's levels by
complete random assignment, so the levels appear in as equal numbers as
possible.

## Usage

``` r
conjoint_assignment(data, levels_list)
```

## Arguments

- data:

  A data frame with one row per profile.

- levels_list:

  A named list with one element per attribute, each a vector of that
  attribute's levels, e.g.
  `list(gender = c("woman", "man"), age = c("young", "old"))`.

## Value

`data` with one column per attribute, named as in `levels_list`.

## Details

See
https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments

## Examples

``` r

set.seed(343)
levels_list <- list(gender = c("woman", "man"), age = c("young", "old"))

# 25 subjects, each choosing between two profiles in each of four tasks
profiles <- expand.grid(profile = 1:2, task = 1:4, subject = 1:25)

head(conjoint_assignment(profiles, levels_list))
#>   profile task subject gender   age
#> 1       1    1       1    man   old
#> 2       2    1       1  woman   old
#> 3       1    2       1    man young
#> 4       2    2       1    man   old
#> 5       1    3       1  woman young
#> 6       2    3       1  woman   old
```
