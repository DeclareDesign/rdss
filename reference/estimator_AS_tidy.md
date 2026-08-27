# Tidy helper function for estimator_AS function

Runs estimates estimation function from interference package and returns
tidy data frame output

## Usage

``` r
estimator_AS_tidy(data, p_matrix, adj_matrix, obs_prob_exposure)
```

## Arguments

- data:

  A `data.frame` containing the observed data.

- adj_matrix:

  An adjacency matrix defining the network structure. This can be
  created, for example, as follows:


        adjacency <- fairfax |>
          as("Spatial") |>
          spdep::poly2nb(queen = TRUE) |>
          spdep::nb2mat(style = "B", zero.policy = TRUE)
        

- obs_prob_exposure:

  A set of exposure probabilities. These can be generated, for example,
  using:


        prob_exposure <- interference::make_exposure_prob(
          potential_tr_vector = permutations,
          adj_matrix = adjacency,
          exposure_map_fn = interference::make_exposure_map_AS,
          exposure_map_fn_add_args = list(hop = 1)
        )
        

- permutation_matrix:

  A matrix of random treatment assignments. Each row corresponds to one
  permutation of the treatment vector.

## Value

a data.frame of estimates

## Details

The estimator_AS_tidy function requires the 'interference' package,
which is not yet available on CRAN.

To use this function:

1.  install the developer version of interference via
    remotes::install_github('szonszein/interference') and

2.  install the developer version of rdss via
    remotes::install_github('DeclareDesign/rdss@remotes')

See
https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
