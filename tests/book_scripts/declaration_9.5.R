library(DeclareDesign); library(rdddr); library(tidyverse)


base_declaration <-
  declare_model(N = 100, 
                age = round(rnorm(N, mean = true_mean, sd = 23))) +
  declare_inquiry(mean_age = mean(age)) +
  declare_sampling(S = complete_rs(N = N, n = 3)) +
  declare_estimator(age ~ 1, model = lm_robust) 
declaration_9.5 <- redesign(base_declaration, true_mean = seq(0, 100, length.out = 10))
