# Tidy helper function for estimator_AS function

Runs estimates estimation function from interference package and returns
tidy data frame output

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

  A `data.frame` containing the observed data.

- permutatation_matrix:

  A matrix of random treatment assignments. Each column corresponds to
  one permutation of the treatment vector, as returned by
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

a data.frame of estimates

## Details

The estimator_AS_tidy function requires the 'interference' package,
which is not available on CRAN.

To use this function, install it with
remotes::install_github('szonszein/interference')

Without it the function returns nothing and explains why, so a design
that includes it still declares and diagnoses.

See
https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
