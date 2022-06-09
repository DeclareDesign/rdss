print('declaration_23.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


# compare status quo to a new proposed policy, 
# given cost of switching 
N <- 100
effect_size <- 0.1
declaration_23.1 <-
  declare_model(N = N,
                U = rnorm(N),
                potential_outcomes(Y ~ effect_size * Z + U)) +
  declare_inquiry(
    ATE = mean(Y_Z_1 - Y_Z_0),
    alternative_better_than_sq = if_else(ATE > 0.1, TRUE, FALSE)
  ) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z,
                    .method = difference_in_means,
                    inquiry = "ATE",
                    label = "dim") +
  declare_estimator(Y ~ Z,
                    .method = lh_robust,
                    linear_hypothesis = "Z - 0.05 = 0",
                    inquiry = "alternative_better_than_sq",
                    label = "decision")
