print('declaration_9.3.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_9.3 <-
  declare_model(data = clingingsmith_etal,
                # this is the sharp null hypothesis
                potential_outcomes(Y ~ 0 * Z + views)) +
  declare_assignment(Z = complete_ra(N = N, m = sum(success))) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z)
