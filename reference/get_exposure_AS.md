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

## Examples

``` r

set.seed(343)
# A ring of 20 units, each adjacent to the units on either side of it
N <- 20
adjacency <- matrix(0, N, N)
adjacency[cbind(1:N, c(2:N, 1))] <- 1
adjacency <- adjacency + t(adjacency)
Z <- randomizr::complete_ra(N)

exposure <- interference::make_exposure_map_AS(
  adj_matrix = adjacency, tr_vector = Z, hop = 1
)
get_exposure_AS(exposure)
#>  [1] "dir_ind1" "dir_ind1" "ind1"     "isol_dir" "ind1"     "isol_dir"
#>  [7] "ind1"     "no"       "no"       "ind1"     "dir_ind1" "dir_ind1"
#> [13] "dir_ind1" "ind1"     "ind1"     "isol_dir" "ind1"     "ind1"    
#> [19] "dir_ind1" "dir_ind1"
```
