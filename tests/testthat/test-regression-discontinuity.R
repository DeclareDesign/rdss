
test_that("tidy.rdrobust and rdrobust_helper work", {

  skip_if_not_installed("rdrobust")

  library(dplyr)
  library(DeclareDesign)
  library(rdrobust)

  set.seed(42)

  cutoff <- 0.5
  control <- function(X) {
    as.vector(poly(X, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
  treatment <- function(X) {
    as.vector(poly(X, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}

  declaration_16.5 <-
    declare_model(
      N = 500,
      U = rnorm(N, 0, 0.1),
      X = runif(N, 0, 1) + U - cutoff,
      D = 1 * (X > 0),
      Y_D_0 = control(X) + U,
      Y_D_1 = treatment(X) + U
    ) +
    declare_inquiry(LATE = treatment(0.5) - control(0.5)) +
    declare_measurement(Y = reveal_outcomes(Y ~ D)) +
    declare_estimator(
      Y, X, c = 0,
      term = "Bias-Corrected",
      .method = rdrobust_helper,
      inquiry = "LATE",
      label = "optimal"
    )
  expect_error(simulate_design(declaration_16.5, sims = 1, future.seed = FALSE), NA)

})
