## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------
# Meta-analysis
# -------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
    model = rma_estimator, model_summary = rma_mu_tau,
    term = c("mu", "tau_sq"), inquiry = c("mu", "tau_sq"),
    label = "random-effects")


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, model = c("random-effects", "fixed-effects"))

simulations <- simulate_design(designs, sims = sims)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
declare_estimator(
  yi = estimate, sei = std.error, method = "FE",
  model = rma_estimator, model_summary = rma_mu_tau,
  term = c("mu", "tau_sq"), inquiry = c("mu", "tau_sq"),
  label = "fixed-effects")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------
# Meta-analysis :: Borrowing strength to improve study-level estimates
# --------------------------------------------------------------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
re_model_code <- "
   data {
     int<lower=0> J;         // number of sites
     real y[J];              // estimated effects
     real<lower=0> sigma[J]; // s.e. of effect estimates
   }
   parameters {
     real mu;
     real<lower=0> tau;
     real eta[J];
   }
   transformed parameters {
     real theta[J];
     real tau_sq = tau^2;
     for (j in 1:J)
       theta[j] = mu + tau * eta[j];
   }
   model {
     target += normal_lpdf(eta | 0, 1);
     target += normal_lpdf(y | theta, sigma);
   }
"

re_model <- stan_model(model_code = re_model_code)

study_level_estimators <- 
  declare_estimator(
    site = 1:10, estimate = estimate[1:10], std.error = std.error[1:10], 
    handler = summarize, estimator = "study_estimate", inquiry = "theta") + 
  declare_estimator(
    model = re_model, handler = label_estimator(stan_re_estimator_theta), 
    inquiry = "theta", label = "stan")

