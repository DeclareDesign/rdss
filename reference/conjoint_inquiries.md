# Conjoint experiment inquiries handler

See
https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments

## Usage

``` r
conjoint_inquiries(data, levels_list, utility_fn)
```

## Arguments

- data:

  A data.frame

- levels_list:

  List of conjoint levels

- utility_fn:

  a function that takes data and returns an additional column called U,
  which represents the utility of the choice

## Value

a data.frame of estimand values
