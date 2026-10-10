# Tidy helper function for estimator_AS function

Runs the Aronow and Samii estimator from the 'interference' package and
returns Horvitz-Thompson and Hajek estimates of the total, direct, and
indirect effects. Exposure is defined by one-hop neighbours in
`adj_matrix`. The data must contain the treatment as `Z` and the outcome
as `Y`.

## Usage

``` r
estimator_AS_tidy(
  data,
  permutatation_matrix = NULL,
  adj_matrix,
  obs_prob_exposure = NULL
)
```

## Arguments

- data:

  A data frame with columns `Z` and `Y`.

- permutatation_matrix:

  (The spelling is the published one and is kept so existing code runs.)
  A matrix of random treatment assignments with one row per permutation
  of the treatment vector and one column per unit, as returned by
  `t(obtain_permutation_matrix(declaration))`. The exposure
  probabilities are computed from it unless `obs_prob_exposure` is
  supplied.

- adj_matrix:

  An adjacency matrix defining the network structure. This can be
  created, for example, as follows:


      adjacency <- fairfax |>
        as("Spatial") |>
        spdep::poly2nb(queen = TRUE) |>
        spdep::nb2mat(style = "B", zero.policy = TRUE)

- obs_prob_exposure:

  Optional exposure probabilities. They depend only on the permutation
  matrix and the network, so in a simulation they are the same on every
  draw; computing them once and passing them here avoids recomputing
  them each time. For example:


      prob_exposure <- interference::make_exposure_prob(
        potential_tr_vector = permutatation_matrix,
        adj_matrix = adjacency,
        exposure_map_fn = interference::make_exposure_map_AS,
        exposure_map_fn_add_args = list(hop = 1)
      )

  When `NULL`, the default, they are computed from
  `permutatation_matrix`.

## Value

A data frame with six rows (three inquiries by two estimators) and
columns `term`, `inquiry`, `estimator`, and `estimate`. No standard
errors are returned.

## Details

The function requires the 'interference' package, which is not available
on CRAN.

To use this function, install it with
remotes::install_github('szonszein/interference')

Without it the function returns nothing and explains why, so a design
that includes it still declares and diagnoses.

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

# 100 possible assignments, one per row
permutations <- t(replicate(100, randomizr::complete_ra(N)))

dat <- data.frame(Z = Z, Y = Z + rnorm(N))
estimator_AS_tidy(dat, permutatation_matrix = permutations, adj_matrix = adjacency)
#> # A tibble: 6 × 4
#>   term     inquiry      estimator        estimate
#>   <chr>    <chr>        <chr>               <dbl>
#> 1 dir_ind1 total_ATE    Horvitz-Thompson     2.08
#> 2 isol_dir direct_ATE   Horvitz-Thompson     1.42
#> 3 ind1     indirect_ATE Horvitz-Thompson     1.53
#> 4 dir_ind1 total_ATE    Hajek                2.11
#> 5 isol_dir direct_ATE   Hajek                1.33
#> 6 ind1     indirect_ATE Hajek                1.46
```
