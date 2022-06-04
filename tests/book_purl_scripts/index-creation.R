library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# --------------
# Index creation
# --------------

## ----file = "scripts_declarations/declaration_14.6.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_14.6 <- diagnose_design(declaration_14.6) 


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -----------------------------------
# Index creation :: Two factor models
# -----------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------

rho <- 0.0

design <-
  declare_model(
    N = 1000,
    draw_multivariate(c(latent_economic, latent_social) ~ mvrnorm(
      n = N,
      mu = c(0, 0),
      Sigma = matrix(c(1, rho, rho, 1), 2, 2)
    ))

  ) +
  declare_inquiry(true_cor = cor(latent_economic, latent_social)) +
  declare_measurement(
    Y_1 = scale(0.1 * latent_economic + 0.9 * latent_social + rnorm(N, sd = 0.2)),
    Y_2 = scale(0.3 * latent_economic + 0.7 * latent_social + rnorm(N, sd = 0.2)),
    Y_3 = scale(0.5 * latent_economic + 0.5 * latent_social + rnorm(N, sd = 0.2)),
    Y_4 = scale(0.7 * latent_economic + 0.3 * latent_social + rnorm(N, sd = 0.2)),
    Y_5 = scale(0.9 * latent_economic + 0.1 * latent_social + rnorm(N, sd = 0.2))
  ) +
  declare_measurement(FA = fa(
    r = cbind(Y_1, Y_2, Y_3, Y_4, Y_5),
    nfactors = 2,
    rotate = "oblimin",
    scores = "tenBerge"
  )$scores) +
   declare_estimator(~FA.MR1 + FA.MR2, model = stats:::cor.test.formula, inquiry = "true_cor")


## ----eval = TRUE----------------------------------------------------------------------------------------
designs <- redesign(design, rho = seq(-1, 1, length.out = 10))
simulations <- simulate_design(designs, sims = 100)



