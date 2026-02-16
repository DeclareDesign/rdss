# Conjoint experiment assignment handler: conducts complete random assignment of all attribute levels

See
https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments

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
