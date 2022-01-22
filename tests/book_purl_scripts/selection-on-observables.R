## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------
# Selection-on-observables
# ------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
exact_matching <- 
  function(data) { 
    matched <- matchit(D ~ X, method = "exact", data = data) 
    match.data(matched) 
  }

design <-
  declare_model(
    N = 100, 
    U = rnorm(N), 
    X = rbinom(N, 1, prob = 0.5),
    D = rbinom(N, 1, prob = 0.25 + 0.5 * X),
    Y_D_0 = 0.2 * X + U,
    Y_D_1 = Y_D_0 + 0.5
  ) + 
  declare_inquiry(ATE = mean(Y_D_1 - Y_D_0)) +
  declare_step(handler = exact_matching) +
  declare_measurement(Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(Y ~ D,
                    weights = weights,
                    model = difference_in_means,
                    label = "adjusted") +
  declare_estimator(Y ~ D, 
                    model = difference_in_means, 
                    label = "unadjusted")


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- diagnose_design(design, sims = sims, bootstrap_sims = b_sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------
# Selection-on-observables :: Exercises {-}
# -----------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(sensemakr)
sensitivity_helper <-
  function(model) {
    sensitivity <- sensemakr(model = model, treatment = "Z")
    sensitivity$sensitivity_stats[,c("estimate", "rv_qa")] 
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_population(N = 1000, 
                     U = rnorm(N, sd = 0.6),
                     X = correlate(rnorm, given = U, rho = 0.5),
                     Z = rbinom(N, 1, prob = pnorm(X)),
                     Y = 0.2 * Z + 0.5 * X + U) +
  declare_estimator(Y ~ Z + X, model = lm, model_summary = sensitivity_helper)

