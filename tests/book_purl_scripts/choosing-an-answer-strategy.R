## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------
# Types of answer strategies :: Point estimation
# ----------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(N = 100, age = sample(0:80, size = N, replace = TRUE)) +
  declare_inquiry(mean_age = mean(age)) +
  declare_sampling(S = complete_rs(N = N, n = 3)) +
  declare_estimator(age ~ 1, model = lm_robust) 


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_design(design, sims = sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------
# Types of answer strategies :: Tests
# -----------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  design +
  declare_test(age ~ 1, 
               linear_hypothesis = "(Intercept) = 20", 
               model = lh_robust, label = "test")


## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
clingingsmith_etal <- read_csv("data/clingingsmith_et_al_2009.csv")

observed_estimate <-
  difference_in_means(views ~ success, data = clingingsmith_etal)

observed_estimate


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
null_model <- 
  declare_model(data = clingingsmith_etal,
                potential_outcomes(Y ~ 0 * Z + views))

null_design <-
  null_model +
  declare_assignment(Z = complete_ra(N = N, m = sum(success))) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z)


## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
p.value <-
  declare_diagnosands(
    p.value = mean(abs(estimate) >= abs(observed_estimate$coefficients))
  )

nhst <-
  diagnose_design(null_design, diagnosands = p.value, sims = 1000)

get_diagnosands(nhst)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------
# Types of answer strategies :: Bayesian formalizations
# -----------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rstanarm)
library(broom.mixed)

design <-
  declare_model(N = 100, age = sample(0:80, size = N, replace = TRUE)) +
  declare_inquiry(mean_age = mean(age)) +
  declare_sampling(S = complete_rs(N = N, n = 3)) +
  declare_estimator(
    age ~ 1,
    model = stan_glm,
    family = gaussian(link = "log"),
    prior_intercept = normal(50, 5),
    model_summary = ~tidy(., exponentiate = TRUE),
    inquiry = "mean_age"
  )


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_design(design, sims = sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------
# Types of answer strategies :: Interval estimation
# -------------------------------------------------

## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
lower_bound <- (3 * 15 + 97 * 0)/100
upper_bound <- (3 * 15 + 97 * 110)/100
 c(lower_bound, upper_bound)


## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
lower_bound <- (90 * 44 + 10 * 0)/100
upper_bound <- (90 * 44 + 10 * 110)/100
c(lower_bound, upper_bound)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------
# Uncertainty
# -----------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
three_italian_citizens <- fabricate(N = 3, age = c(5, 15, 25))
answer_strategy <- declare_estimator(age ~ 1)
answer_strategy(three_italian_citizens)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(N = 100, 
                age = round(rnorm(N, mean = true_mean, sd = 23))) +
  declare_inquiry(mean_age = mean(age)) +
  declare_sampling(S = complete_rs(N = N, n = 3)) +
  declare_estimator(age ~ 1, model = lm_robust) 


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, true_mean = seq(0, 100, length.out = 10))


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------
# Applying design principles :: Seek M:I::D:A parallelism
# -------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MI <-
  declare_model(
    N = 100,
    X = rbinom(N, size = 1, 0.5),
    U = rnorm(N),
    potential_outcomes(Y ~ 0.5 * Z+-0.5 * X + 0.5 * X * Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

D1 <- declare_assignment(Z = complete_ra(N = N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))
D2 <- declare_assignment(Z = block_ra(blocks = X, block_prob = c(0.1, 0.8))) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))


A1 <- declare_estimator(Y ~ Z, label = "Unweighted")
A2 <-
  declare_step(
    handler = fabricate,
    ipw = 1 / obtain_condition_probabilities(
      assignment = Z,
      blocks = X,
      block_prob = c(0.1, 0.8)
    )
  ) +
  declare_estimator(Y ~ Z, weights = ipw, label = "Weighted")

designs <- list(MI + D1 + A1,
                MI + D1 + A2,
                MI + D2 + A1,
                MI + D2 + A2)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------
# Declaring answer strategies in code :: Statistical modeling functions
# ---------------------------------------------------------------------

## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(
    N = 100, U = rnorm(N), potential_outcomes(Y ~ 0.2 * Z + U)
  ) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_estimator(model = lm_robust, 
                    formula = Y ~ Z, 
                    model_summary = tidy, 
                    term = "Z", 
                    inquiry = "ATE", 
                    label = "lm_no_controls")


## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
draw_estimates(design)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
# Declaring answer strategies in code :: Tidying statistical modelling function output
# ------------------------------------------------------------------------------------

## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
A <- declare_estimator(Y ~ Z, 
                       model = lm_robust, 
                       model_summary = glance)
A(draw_data(design))


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tidy_lm <- function(fit) {
  # calculate estimates by grabbing the coefficients from the model fit
  estimate <- coef(lm)
  
  # get the names of the coefficients (e.g., "(Intercept)", "Z")
  #   we will call these "term" to represent regression terms
  term <- names(estimates)
  
  # calculate the standard error by grabbing the variance-covariance
  #   matrix, then pulling the diagonal elements of it and taking the 
  #   square root to transform from variances to standard errors
  std.error <- sqrt(diag(vcov(lm)))
  
  # return a tibble with term, estimate, and std.error
  tibble(term = term, estimate = unlist(estimate), std.error = std.error)
}

declare_estimator(
  Y ~ Z
  model = lm,
  model_summary = tidy_lm
)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tidy_margins <- function(x) {
  tidy(margins(x, data = x$data), conf.int = TRUE)
}

declare_estimator(
  Y ~ Z + X,
  model = glm,
  family = binomial("logit"),
  model_summary = tidy_margins,
  term = "Z"
) 


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------
# Declaring answer strategies in code :: Custom answer strategies
# ---------------------------------------------------------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
my_estimator <- function(data) {
  data.frame(estimate = mean(data$Y))
}
declare_estimator(handler = label_estimator(my_estimator),
                  label = "mean",
                  inquiry = "Y_bar")

