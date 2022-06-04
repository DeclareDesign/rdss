library(DeclareDesign); library(rdddr); library(tidyverse)


model <- "fixed-effects"
design <-
  declare_model(
    N = 100,
    site = 1:N,
    mu = 0.2,
    tau = case_when(model == "random-effects" ~ 1,
                    model == "fixed-effects" ~ 0),
    std.error = pmax(0.1, abs(rnorm(N, mean = 0.8, sd = 0.5))),
    eta = rnorm(N),
    theta = mu + tau * eta, # note when tau = 0, theta = mu 
    estimate = rnorm(N, mean = theta, sd = std.error)
  ) + 
  declare_inquiry(mu = first(mu), tau_sq = first(tau^2)) + 
  declare_estimator(
    yi = estimate, sei = std.error, method = "REML",
    model = rma_helper, model_summary = rma_mu_tau,
    term = c("mu", "tau_sq"), inquiry = c("mu", "tau_sq"),
    label = "random-effects") + 
  declare_estimator(
    yi = estimate, sei = std.error, method = "FE",
    model = rma_helper, model_summary = rma_mu_tau,
    term = c("mu", "tau_sq"), inquiry = c("mu", "tau_sq"),
    label = "fixed-effects")
declaration_18.4 <- redesign(design, model = c("random-effects", "fixed-effects"))
