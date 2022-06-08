print('declaration_21.x2.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_21.x2 <-
  declare_model(N = 100,
                U = rnorm(N),
                potential_outcomes(Y ~ 0.1 * Z + U)) +
  declare_inquiry(SATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "SATE")
