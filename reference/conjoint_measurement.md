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
