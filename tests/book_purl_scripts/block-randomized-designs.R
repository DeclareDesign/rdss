## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------
# Block-randomized experiments
# ----------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_assignment(
    Z = block_ra(blocks = X, block_prob = c(0.2, 0.5)),
    probs =
      obtain_condition_probabilities(Z, blocks = X, 
                                     block_prob = c(0.2, 0.5)),
    ipw = 1 / probs
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    covariates = ~ X,
    model = lm_lin,
    weights = ipw,
    label = "Lin"
  )


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------
# Block-randomized experiments :: Can blocking ever hurt?
# -------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MI <- declare_model(
  N = 100,
  X = sort(rnorm(N)),
  couple = c(1:(N / 2), (N / 2):1),
  U = rnorm(N, sd = 0.1),
  potential_outcomes(Y ~ Z + X * Z + U)
) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

design_complete <- MI +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z)

design_blocked <- MI +
  declare_assignment(Z = block_ra(blocks = couple)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_designs(design_complete, design_blocked)


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_designs(design_complete, design_blocked)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# Block-randomized experiments :: Simulation comparing blocking to covariate adjustment
# -------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fixed_pop <-
  fabricate(
    N = 100,
    X = rbinom(N, 1, 0.5),
    U = rnorm(N),
    potential_outcomes(Y ~ 0.2*Z + X + U)
  )

MI <-
  declare_model(data = fixed_pop) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data strategies
complete_assignment <- 
  declare_assignment(Z = complete_ra(N = N)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z))
blocked_assignment <- 
  declare_assignment(Z = block_ra(blocks = X)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z))

# Answer strategies
unadjusted_estimator <- declare_estimator(Y ~ Z, inquiry = "ATE")
adjusted_estimator <- declare_estimator(Y ~ Z + X, model = lm_robust, inquiry = "ATE")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design_1 <- MI + complete_assignment + unadjusted_estimator
design_2 <- MI + blocked_assignment + unadjusted_estimator
design_3 <- MI + complete_assignment + adjusted_estimator
design_4 <- MI + blocked_assignment + adjusted_estimator


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_designs(list(design_1, design_2, design_3, design_4))


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Block-randomized experiments :: Can controlling for covariates hurt precision?
# ------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prob = 0.5
control_slope = -1

design <-
  declare_model(N = 100,
                X = runif(N, 0, 1),
                U = rnorm(N, sd = 0.1),
                Y_Z_1 = 1*X + U,
                Y_Z_0 = control_slope*X + U
                ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, prob = prob)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + X, model = lm_robust, inquiry = "ATE", label = "OLS") +
  declare_estimator(Y ~ Z, covariates = ~X, model = lm_lin, inquiry = "ATE", label = "Lin")



## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, 
                    control_slope = seq(-1, 1, 0.5), 
                    prob = seq(0.1, 0.9, 0.1))

simulations <- simulate_designs(designs)


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, 
                    control_slope = seq(-1, 1, 0.5), 
                    prob = seq(0.1, 0.9, 0.1))

simulations <- simulate_designs(designs)

