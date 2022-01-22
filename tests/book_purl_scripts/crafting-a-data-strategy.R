## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------
# Declaring data strategies in code :: Sampling
# ---------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
D <- declare_sampling(S = complete_rs(N = 100, n = 10))


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
D <-
  declare_sampling(
    # sample 20 districts
    S_districts = cluster_rs(clusters = districts, n = 20),
    # within each district, sample 50 villages
    S_villages  = strata_and_cluster_rs(
      strata = districts,
      clusters = villages,
      strata_n = 10
    ),
    # within each village select 25 households
    S_households  = strata_and_cluster_rs(
      strata = villages,
      clusters = households,
      strata_n = 25
    ),
    S = S_districts == 1 & S_villages == 1 & S_households == 1
    filter = S == 1
  )


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
D <-
  declare_sampling(S = cluster_rs(clusters = districts, n = 20)) +
  declare_sampling(S = strata_and_cluster_rs(
    strata = districts,
    clusters = villages,
    strata_n = 10
  )) +
  declare_sampling(S = strata_and_cluster_rs(
    strata = villages,
    clusters = households,
    strata_n = 25
  ))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
M <-
  declare_model(N = 100,
                X = rbinom(N, 1, prob = 0.5))

D <-
  declare_sampling(
    S = strata_rs(strata = X, strata_prob = c(0.2, 0.5)),
    S_inclusion_probability =
      strata_rs_probabilities(strata = X,
                              strata_prob = c(0.2, 0.5))
  )


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------
# Declaring data strategies in code :: Treatment assignment
# ---------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
D <- 
  declare_assignment(
    Z = complete_ra(N, m = 50),
    Z_condition_probability = 
      obtain_condition_probabilities(assignment = Z, m = 50)
  )


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------
# Declaring data strategies in code :: Measurement
# ------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
M <- declare_model(N = 100, latent = runif(N))
D <- declare_measurement(observed = rbinom(N, 1, prob = latent))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
M <-
  declare_model(N = 100,
                potential_outcomes(Y ~ rbinom(
                  N, size = 1, prob = 0.1 * Z + 0.5
                )))

D <-
  declare_assignment(Z = complete_ra(N, m = 50)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))


## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(psych)

D <- declare_measurement(
  index = fa(
    r = cbind(Y_1, Y_2, Y_3),
    nfactors = 1,
    rotate = "varimax"
  )$scores
)

