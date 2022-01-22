## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------
# Cluster random sampling
# -----------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(343)
ICC <- 0.4

two_nigerian_states <-
  fabricate(
    state = add_level(N = 2, 
                      state_name = c("taraba", "kwara"),
                      state_mean = c(-0.2, 0.2)),
    locality = add_level(
      N = 500,
      locality_shock = rnorm(N, state_mean, sqrt(ICC))
    ),
    individual = add_level(
      N = 100,
      individual_shock = rnorm(N, sd = sqrt(1 - ICC)),
      Y_star = locality_shock + individual_shock
    )
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
budget_function <- 
  function(cluster_prob){
  budget = 20000
  cluster_cost = 20
  individual_cost = 2
  n_clusters = 1000
  n_individuals_per_cluster = 100
  
  total_cluster_cost <-
    cluster_prob * n_clusters * cluster_cost
  
  remaining_funds <- budget - total_cluster_cost
  
  sampleable_individuals <- 
    cluster_prob * n_clusters * n_individuals_per_cluster
  
  individual_prob = 
    (remaining_funds/individual_cost)/sampleable_individuals
  
  pmin(individual_prob, 1)
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(data = two_nigerian_states) +
  declare_measurement(Y = as.numeric(cut(Y_star, 7))) +
  declare_inquiry(Y_bar = mean(Y)) +
  declare_sampling(
    S_cluster = strata_and_cluster_rs(
      strata = state,
      clusters = locality,
      prob = cluster_prob
    ),
    filter = S_cluster == 1
  ) +
  declare_sampling(
    S_individual = 
      strata_rs(strata = locality, 
                prob = budget_function(cluster_prob)),
    filter = S_individual == 1
  ) +
  declare_estimator(Y ~ 1,
                    clusters = locality,
                    inquiry = "Y_bar")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, cluster_prob = seq(0.1, 0.9, 0.1))
diagnosis <- diagnose_design(designs, sims = 500)

