# Tidy helper function for estimator_AS function

Runs estimates estimation function from interference package and returns
tidy data frame output

## Usage

``` r
estimator_AS_tidy(data, permutatation_matrix, adj_matrix)
```

## Arguments

- data:

  A `data.frame` containing the observed data.

- permutatation_matrix:

  A matrix of random treatment assignments. Each column corresponds to
  one permutation of the treatment vector, as returned by
  `t(obtain_permutation_matrix(declaration))`. The exposure
  probabilities are computed from it.

- adj_matrix:

  An adjacency matrix defining the network structure. This can be
  created, for example, as follows:


      adjacency <- fairfax |>
        as("Spatial") |>
        spdep::poly2nb(queen = TRUE) |>
        spdep::nb2mat(style = "B", zero.policy = TRUE)

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
