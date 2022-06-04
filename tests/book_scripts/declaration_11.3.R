library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_11.3 <-
  declare_model(
    N = 100,
    X = runif(N, 0, 3)) +
  declare_inquiry(handler = cef_inquiry) +
  declare_measurement(Y = dip(X) + rnorm(N, 0, .5)) +
  declare_estimator(handler = cef_estimator)
