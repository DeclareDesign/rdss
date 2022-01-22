## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------
# Simple random sampling
# ----------------------

## ----tmpasdf-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(343)
portola <-
  fabricate(
    N = 2100,
    Y_star = rnorm(N)
    )

design <- 
  declare_model(data = portola) + 
  declare_measurement(Y = as.numeric(cut(Y_star, 7))) + 
  declare_inquiry(Y_bar = mean(Y)) + 
  declare_sampling(S = complete_rs(N, n = 100)) + 
  declare_estimator(Y ~ 1, inquiry = "Y_bar")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2))
)
diagnosis <- diagnose_design(design, diagnosands = diagnosands) 


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------
# Simple random sampling :: What can go wrong
# -------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(data = portola) + 
  declare_measurement(Y = as.numeric(cut(Y_star, 7))) + 
  declare_inquiry(Y_bar = mean(Y)) + 
  declare_sampling(S = complete_rs(N, n = 100)) + 
  declare_measurement(
    R = rbinom(n = N, size = 1, prob = pnorm(Y_star + effort)),
    Y = if_else(R == 1, Y, NA_real_)
  ) +
  declare_estimator(Y ~ 1, inquiry = "Y_bar") +
  declare_estimator(R ~ 1, label = "Response Rate")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, effort = seq(0, 5, by = 0.5))
diagnosis <- diagnose_designs(designs, sims = 500)

