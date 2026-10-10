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

  A data.frame

- utility_fn:

  a function that takes data and returns an additional column called U,
  which represents the utility of the choice

## Value

a data.frame

## Details

See
https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
