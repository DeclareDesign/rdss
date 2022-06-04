library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_21.x <-
  declare_model(N = 100,
                U = rnorm(N, sd = 0.2),
                potential_outcomes(Y ~ 0.15 * Z + U)) +
  declare_assignment(Z = complete_ra(N = N, prob = prob)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z)
