## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ----------------------
# Simple random sampling
# ----------------------

## ----file = "scripts_declarations/declaration_14.1.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2))
)
diagnosis_14.1 <- diagnose_design(declaration_14.1, diagnosands = diagnosands) 


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# -------------------------------------------
# Simple random sampling :: What can go wrong
# -------------------------------------------

## ----file = "scripts_declarations/declaration_14.2.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosis_14.2 <- 
  declaration_14.2 %>% 
  redesign(effort = seq(0, 5, by = 0.5)) %>% 
  diagnose_designs()


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# -----------------------
# Cluster random sampling
# -----------------------

## ----file = "scripts_declarations/declaration_14.3a.R"--------------------------------------------------------------------------------


## ----file = "scripts_declarations/declaration_14.3b.R"--------------------------------------------------------------------------------


## ----file = "scripts_declarations/declaration_14.3c.R"--------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
designs <- redesign(declaration_14.3, 
                    cluster_prob = seq(0.1, 0.9, 0.1))
diagnosis_14.3 <- diagnose_design(designs)


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------
# Multi-level regression and poststratification
# ---------------------------------------------

## ----file = "scripts_declarations/declaration_14.4.R"---------------------------------------------------------------------------------


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# Multi-level regression and poststratification :: Redesign over answer strategies
# --------------------------------------------------------------------------------

## ----file = "scripts_declarations/declaration_14.5.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosis_14.4 <- diagnose_design(declaration_14.5)


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# --------------
# Index creation
# --------------

## ----file = "scripts_declarations/declaration_14.6.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosis_14.6 <- diagnose_design(declaration_14.6) 


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# -----------------------------------
# Index creation :: Two factor models
# -----------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------

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


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, rho = seq(-1, 1, length.out = 10))
simulations <- simulate_design(designs, sims = 100)


