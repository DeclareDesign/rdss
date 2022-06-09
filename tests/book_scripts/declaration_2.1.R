print('declaration_2.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


prefs_for_A <- 0.51
trend <- 0 # steady!
N <- 1000
# declaring the model and inquiry
steady_race <-
  declare_model(
    N = N,
    Y_time_1 = rbinom(N, size = 1, prob = prefs_for_A + 0 * trend),
    Y_time_2 = rbinom(N, size = 1, prob = prefs_for_A + 1 * trend),
    Y_time_3 = rbinom(N, size = 1, prob = prefs_for_A + 2 * trend)
  ) +
  declare_inquiry(
    A_voteshare = 
      rnorm(n = 1, mean = prefs_for_A + 3 * trend, sd = 0.01))
# declaring the data and answer strategies
single_poll <-
  declare_measurement(Y_obs = Y_time_1) +
  declare_estimator(Y_obs ~ 1, .method = lm_robust)
# declaring the full design
declaration_2.1 <- steady_race + single_poll
