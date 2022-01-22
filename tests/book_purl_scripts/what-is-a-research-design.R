## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------
# Example: October surprise :: A "steady race" design
# ---------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prefs_for_A <- 0.51
trend <- 0 # steady!
N <- 1000

# declaring the model and inquiry
steady_race <-
  declare_model(
    N = N,
    Y_time_1 = rbinom(N, size = 1, prob = prefs_for_A + 0 * trend),
    Y_time_2 = rbinom(N, size = 1, prob = prefs_for_A + 1 * trend),
    Y_time_3 = rbinom(N, size = 1, prob = prefs_for_A + 2 * trend)
  ) +
  declare_inquiry(
    A_voteshare = 
      rnorm(n = 1, mean = prefs_for_A + 3 * trend, sd = 0.01))

# declaring the data and answer strategies
single_poll <-
  declare_measurement(Y_obs = Y_time_1) +
  declare_estimator(Y_obs ~ 1, model = lm_robust)

# declaring the full design
steady_race_design <- steady_race + single_poll


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosands <-
  declare_diagnosands(
    correct_call_rate = mean((estimate > 0.5) == (estimand > 0.5))
  )

diagnosis <-
  diagnose_design(design = steady_race_design,
                  diagnosands = diagnosands)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------
# Example: October surprise :: An "October surprise" design
# ---------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(modelr)

trend <- -0.01 # "surprising" trend away from candidate A

# Declaring the new model and inquiry
october_surprise <- 
  declare_model(
    N = N,
    Y_time_1 = rbinom(N, size = 1, prob = prefs_for_A + 0 * trend),
    Y_time_2 = rbinom(N, size = 1, prob = prefs_for_A + 1 * trend),
    Y_time_3 = rbinom(N, size = 1, prob = prefs_for_A + 2 * trend)
  ) +
  declare_inquiry(
    A_voteshare = 
      rnorm(n = 1, mean = prefs_for_A + 3 * trend, sd = 0.01))


# Declaring the new data and answer strategies
three_polls <-
  declare_assignment(time = complete_ra(N, conditions = 1:3)) +
  declare_measurement(Y_obs = reveal_outcomes(Y ~ time)) +
  declare_estimator(
    Y_obs ~ time,
    model = lm_robust,
    model_summary = ~add_predictions(model = ., 
                                     data = data.frame(time = 4), 
                                     var = "estimate")
  ) 

october_surprise_design <- october_surprise + three_polls


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <-
  diagnose_design(design = october_surprise_design,
                  diagnosands = diagnosands)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------
# Example: October surprise :: Choosing between empirical designs
# ---------------------------------------------------------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- 
  diagnose_design(
     design_1 = steady_race + single_poll,
     design_2 = steady_race + three_polls,
     design_3 = october_surprise + single_poll,
     design_4 = october_surprise + three_polls,
    diagnosands = diagnosands
  )

