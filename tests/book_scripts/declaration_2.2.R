print('declaration_2.2.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(modelr) # required for add_predictions
prefs_for_A <- 0.51
trend <- -0.01 # "surprising" trend away from candidate A
N <- 1000
# Declaring the new model and inquiry
october_surprise <- 
  declare_model(
    N = N,
    Y_time_1 = rbinom(N, size = 1, prob = prefs_for_A + 0 * trend),
    Y_time_2 = rbinom(N, size = 1, prob = prefs_for_A + 1 * trend),
    Y_time_3 = rbinom(N, size = 1, prob = prefs_for_A + 2 * trend)
  ) +
  declare_inquiry(
    A_voteshare = 
      rnorm(n = 1, mean = prefs_for_A + 3 * trend, sd = 0.01))
# Declaring the new data and answer strategies
three_polls <-
  declare_assignment(time = complete_ra(N, conditions = 1:3)) +
  declare_measurement(Y_obs = reveal_outcomes(Y ~ time)) +
  declare_estimator(
    Y_obs ~ time,
    .method = lm_robust,
    .summary = ~add_predictions(method = ., 
                                data = data.frame(time = 4), 
                                var = "estimate")
  ) 
declaration_2.2 <- october_surprise + three_polls
