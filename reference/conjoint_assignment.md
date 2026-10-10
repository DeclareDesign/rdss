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
