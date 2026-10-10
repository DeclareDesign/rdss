# Helper function to obtain the observed exposure for the Aronow and Samii estimator

Converts the exposure map from
[`interference::make_exposure_map_AS()`](https://rdrr.io/pkg/interference/man/make_exposure_map_AS.html),
a 0/1 matrix with one row per unit and one column per exposure
condition, into a vector naming each unit's condition (e.g. `"dir_ind1"`
or `"no"`).

## Usage

``` r
get_exposure_AS(obs_exposure)
```

## Arguments

- obs_exposure:

  An exposure map, as returned by
  [`interference::make_exposure_map_AS()`](https://rdrr.io/pkg/interference/man/make_exposure_map_AS.html).

## Value

A character vector with one element per unit.

## Details

See
https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
