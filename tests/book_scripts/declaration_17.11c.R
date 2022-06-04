library(DeclareDesign); library(rdddr); library(tidyverse)


placebo_controlled_design <-
  MI +
  declare_sampling(S = complete_rs(N)) +
  declare_assignment(Z = conduct_ra(N = N)) +
  declare_measurement(X = if_else(type == "Complier", 1, 0),
                      D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ Z,
    subset = X == 1,
    model = lm_robust,
    inquiry = "CACE",
    label = "OLS among compliers"
  )
