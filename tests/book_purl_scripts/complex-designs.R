## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------
# Mixed methods
# -------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(CausalQueries)
causal_model <- make_model("X -> M -> Y; M <-> Y") %>%
  set_priors(node = "X", alpha = c(100, 100)) %>%
  set_restrictions(labels = list(M = "10",  Y = "10"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Quantitative data
data_handler = function(data, n = n) 
    CausalQueries::make_data(causal_model, 
                             parameters = data$parameters, 
                             n = n)

# Case selection data targets within X, Y data combinations
strategy <- c("00" = 10, "01" = 0, "10" = 0, "11" = 10)
strategy_names <- names(strategy)

# strat_n flexible to take account of the possibility of no data in some strata
strata_n <- function(strategy, strata) 
  sapply(1:4, function(i) min(strategy[i], sum(strata == strategy_names[i])))[strategy_names %in% strata]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimation_handler = function(data) 
		causal_model %>% update_model(data = data) %>%
    query_model(query = "Y[X=1] - Y[X=0]", 
                using = "posteriors", 
                given = c(TRUE, "X==1 & Y==1")) %>%
  rename(estimate = mean) %>%
  select(estimate, sd) %>% 
  mutate(Inquiry = c("ATE", "POC"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
n <- 50

design <-

  declare_model(data.frame(parameters = CausalQueries::get_parameters(causal_model, param_type = "prior_draw"))) +
  
  declare_inquiry(ATE = CausalQueries::query_model(causal_model, "Y[X=1] - Y[X=0]", 
  parameters = parameters, using = "parameters")$mean) +   
  
  declare_inquiry(POC = CausalQueries::query_model(causal_model, "Y[X=1] - Y[X=0]", given = "X==1 & Y==1",  
  parameters = parameters, using = "parameters")$mean) +
  
  declare_measurement(handler = data_handler, n = n) +
  
  declare_measurement(
    strata = paste0(X,Y),
    M_observed = strata_rs(strata = paste0(X,Y), strata_n = strata_n(strategy, strata)),
    M = ifelse(M_observed==1, M, NA)) +
  
  declare_estimator(handler = label_estimator(estimation_handler), inquiry = c("ATE", "POC")) 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mixed_diagnosands <- 
  declare_diagnosands(mean_estimate = mean(estimate),
                      sd_estimate = sd(estimate),
                      bias = mean(estimate - estimand),
                      posterior_variance = mean(sd^2))



## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- design %>%
  redesign(N = c(50, 100, 150), 
           strategy = list(c(0,0,0,0), c(10,0,0,10), c(0,0,0,20))) %>% 
  diagnose_design(sims = 2, diagnosands = mixed_diagnosands)



## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------
# Discovery using causal forests
# ------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
covariates <- paste0("X.", 1:10)

f_Y <- function(z, X.1, X.2, X.3, X.4, u) {
  z * X.1 + z * X.2 ^ 2 + z * exp(X.3) + z * X.3 * X.4 + u
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_predictor <- function(data) {
  data.frame(
    inquiry = "best_predictor",
    estimand = lapply(covariates, function(j) {
      lm_robust(tau ~ cut(data[[j]], 20), data = data)$r.squared
    }) %>%
      unlist %>% which.max
  )
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
causal_forest_handler <- function(data, ...) {

  X <- as.matrix(data %>% select(all_of(covariates)))
  train <- data$train
  
  cf <- causal_forest(X = X[train, ], Y = data$Y[train], W = data$Z[train], ...) 
  
  # Prep and return data
  data$pred <- NA
  data$pred[train]  <- predict(cf, estimate.variance=FALSE)$predictions
  data$pred[!train] <- predict(cf, newdata=X[!train,], estimate.variance=FALSE)$predictions

data %>%
  mutate(var_imp = variable_importance(cf) %>% which.max,
         low_test  = (!train & (pred < quantile(pred[!train], .2))),
         high_test = (!train & (pred > quantile(pred[!train], .8))),
         low_all = pred < quantile(pred, .2))
}

take_1 <-
  function(data) {
    data %>%
      slice(1) %>%
      mutate(estimate = var_imp) %>%
      select(estimate)
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
N <- 1000

design <- 
  declare_model(
    N = N,  
    X = matrix(rnorm(10*N), N),
    u = rnorm(N),
    Z = sample(0:1, N, replace = TRUE)) + 
  declare_measurement(handler = fabricate,
    Y_1 = f_Y(1, X.1, X.2, X.3, X.4, u),
    Y_0 = f_Y(0, X.1, X.2, X.3, X.4, u), 
    tau = Y_1 - Y_0,
    Y = f_Y(Z, X.1, X.2, X.3, X.4, u), 
    train = simple_rs(N)==1) +
  declare_inquiry(handler = best_predictor, label = "custom") +
  declare_step(handler = causal_forest_handler) +
  declare_inquiry(
    worst_effects = mean(tau[tau <= quantile(tau, .2)]),
    weak_effects = mean(tau[low_test]),
    weak_all = mean(tau[low_all]),
    strong_effects = mean(tau[high_test])) +
  declare_estimator(Y ~ Z, model = lm_robust, subset = low_test, 
                    inquiry = c("weak_effects", "worst_effects"), label = "lm_weak") +
  declare_estimator(Y ~ Z, model = lm_robust, subset = low_all, 
                    inquiry = "weak_all", label = "lm_weak_all") +
  declare_estimator(Y ~ Z, model = lm_robust, subset = high_test, 
                    inquiry = "strong_effects", label = "lm_strong") +
  declare_estimator(handler = label_estimator(take_1),
                    inquiry = "best_predictor", label = "cf") 



## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------
# Structural estimation
# ---------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

structural_estimator <- function(data, pi, y, chi = 3/4){
  
  # Define negative log likelihood as a function of k, d and q
  LL  <- function(k, d, q) {
    m <- with(data, y(Z, d))
    R <- q * dbeta(data[pi][[1]], k * chi, k * (1- chi)) +
      (1 - q) * dbeta(data[pi][[1]], k * m, k * (1 - m))
    - sum(log(R))
  }
  
  # Estimation
  M <- mle2(
    LL,
    method = "L-BFGS-B",
    start = list(k = 2, d = 0.50,  q = 0.50),
    lower = c(k = 1,    d = 0.01,  q = 0.01),
    upper = c(k = 1000, d = 0.99,  q = 0.99)
  )
  
  # Format output from estimation
  out <- data.frame(coef(summary(M)), outcome = pi)
  
  names(out) <- c("estimate", "std.error", "statistic", "p.value", "outcome")
  
  # Use estimates of q and delta to predict average treatment effects (ATEs)
  # Predicted ATE for n=2
  out[4, 1] <- (1 - out["q", "estimate"]) * (2 * out["d", "estimate"] - 1)
  
  # Predicted ATE for n=infinity
  out[5, 1] <- (1 - out["q", "estimate"]) * (2 * out["d", "estimate"] /
                                               (1 + out["d", "estimate"]) - 1)
  
  out
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
d = 0.8       # True delta (unknown)
k = 6         # Parameter to governance variance (unknown)
q = 0.5       # Share of behavioral types in the population (unknown)
chi = 0.75    # Price paid by norm following ("behavioral" customers) (known)

design <- 
  
  declare_model(
    
    # Define the population: indicator for behavioral type (norm = 1)
    N = 500, norm = rbinom(N, 1, q),
    
    # Define mean potential outcomes for n = 2 
    potential_outcomes(
      pi_two ~ norm*chi + (1-norm)*(Z*d + (1-Z)*(1-d))
    ),
    
    # Define mean potential outcomes for n = infinity
    potential_outcomes(
      pi_inf ~ norm*chi + (1-norm)*(Z*d/(1+d) + (1-Z)*(1-d/(1+d)))
    )
  ) +
  
  declare_inquiry(ATE_two = mean(pi_two_Z_1 - pi_two_Z_0), # ATE n = 2
                  ATE_inf = mean(pi_inf_Z_1 - pi_inf_Z_0), # ATE n = infinity
                  k = k,                                   # kappa
                  d = d,                                   # delta
                  q = q) +                                 # q
  
  declare_assignment(Z = complete_ra(N)) +
  
  declare_measurement(

    pi_two = reveal_outcomes(pi_two ~ Z),
    pi_inf = reveal_outcomes(pi_inf ~ Z),
    
    # Get draws from beta distribution given means for n = 2 and n = infinity
    pi_two_obs = rbeta(N, pi_two*k, (1-pi_two)*k),      
    pi_inf_obs = rbeta(N, pi_inf*k, (1-pi_inf)*k)
  ) +
  
  # Difference-in-means for n = 2
  declare_estimator(pi_two_obs ~ Z, inquiry = "ATE_two", label = "DIM_two") +
  
  # Difference-in-means for n = infinity
  declare_estimator(pi_inf_obs ~ Z, inquiry = "ATE_inf", label = "DIM_inf") +
  
  # MLE for n = 2
  declare_estimator(handler = tidy_estimator(structural_estimator), 
                    pi = "pi_two_obs", 
                    y = function(Z, d) Z * d + (1 - Z) * (1 - d), 
                    inquiry = c("k","d", "q", "ATE_two", "ATE_inf"), 
                    label = "Struc_two") +
  
  # MLE for n = infinity
  declare_estimator(handler = tidy_estimator(structural_estimator),
                    pi = "pi_inf_obs", 
                    y = function(Z, d) Z*d/(1+d) +  (1-Z)*(1-d/(1+d)),
                    inquiry = c("k","d","q","ATE_two", "ATE_inf"), 
                    label = "Struc_inf") 


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


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------
# Multi-site studies
# ------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimate_study_effects <- function(formula, data) {
  data %>%
    group_by(sites) %>%
    do(tidy(lm_robust(formula, data = .))) %>%
    filter(term == "Z") %>%
    ungroup
}

n_study_sites <- 5
n_subjects_per_site <- 500

design <- 
  declare_model(
    sites = add_level(
      N = 100, 
      study_effect = seq(from = -0.1, to = 0.1, length.out = N) 
    ),
    subjects = add_level(
      N = n_subjects_per_site, 
      U = rnorm(N),
      potential_outcomes(Y ~ Z * (0.1 + study_effect) + U)
    )
  ) +
  declare_inquiry(PATE = mean(Y_Z_1 - Y_Z_0),
                  tau_sq = ) + 
  declare_sampling(S = cluster_rs(clusters = sites, n = n_study_sites)) + 
  declare_sampling(S = strata_rs(strata = sites, n = n_subjects_per_site)) + 
  declare_assignment(Z = block_ra(blocks = sites, prob = 0.5)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_step(Y ~ Z, handler = estimate_study_effects) + 
  declare_estimator(yi = estimate, sei = std.error, method = "REML", 
                    model = rma_estimator, model_summary = rma_mu_tau, inquiry = "PATE", 
                    label = "random-effects")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------
# Multi-site studies :: Coordinating treatments
# ---------------------------------------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
coordination <- 1
prob_select_pos <- .2
n_subjects_per_site <- 2500

design <- 
  declare_model(
    sites = add_level(
      N = 5, 
      tau_1 = rnorm(N, .1),
      tau_2 = rnorm(N),
      tau_3 = rnorm(N),
      compliant = runif(N) < coordination,
      selects_pos = simple_rs(N, prob_select_pos)),
    subjects = add_level(
      N = n_subjects_per_site, 
      tau_mean =  (tau_1 + tau_2 + tau_3)/3,
      tau_max  =  pmax(tau_1, tau_2, tau_3),
      tau_min  =  pmin(tau_1, tau_2, tau_3),
      tau_selected = tau_max*selects_pos + tau_min*(1-selects_pos),
      U = rnorm(N))) +
  declare_inquiry(
    PATE_1 = mean(tau_1),
    PATE_average = mean(tau_mean),
    PATE_best = mean(tau_max),
    PATE_worst = mean(tau_min),
    PATE_selected = mean(tau_selected)
    ) + 
  declare_assignment(Z = block_ra(blocks  = sites)) +
  declare_measurement(
    Y   = Z*(compliant*tau_1 + (!compliant)*tau_selected) + U
    ) +
  declare_step(Y ~ Z, handler = estimate_study_effects) +
  declare_estimator(
    yi = estimate,
    sei = std.error,
    method = "REML",
    model = rma_estimator,
    model_summary = rma_mu_tau,
    inquiry = c(
      "PATE_1",
      "PATE_average",
      "PATE_best",
      "PATE_worst",
      "PATE_selected"
    ),
    label = "random-effects"
  )


## ----echo = TRUE, eval = do_diagnosis & !exists("do_bookdown")-----------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, coordination = (0:4)/4, prob_select_pos = (0:3)/3)
diagnoses <- diagnose_design(designs, sims = sims, bootstrap_sims = 100)

