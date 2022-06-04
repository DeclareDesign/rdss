library(DeclareDesign); library(rdddr); library(tidyverse)


encouragement_design <-
  MI +
  declare_assignment(Z = conduct_ra(N = N)) +
  declare_measurement(D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ D | Z,
    model = iv_robust,
    inquiry = "CACE",
    label = "2SLS among all units"
  )
