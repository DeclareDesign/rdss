## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------
# Process tracing
# ---------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- make_model("X -> M -> Y <- W -> M") %>%
  set_restrictions("(M[X=1] < M[X=0]) | (M[X=1, W=1] == M[X=0, W=1])") %>%
  set_restrictions("(Y[M=1] < Y[M=0]) | (Y[M=1, W=1] == Y[M=0, W=1])")

plot(model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
queries <- 
  CausalQueries::query_model(
    model,
    query = list('Prob(CoE=1)' = "Y[X=1] > Y[X=0]",
                 'Prob(M=1)' = "M==1",
                 'Prob(CoE=1 | M=0)' = "Y[X=1] > Y[X=0]",
                 'Prob(CoE=1 | M=1)' = "Y[X=1] > Y[X=0]"),
    given = list("Y==1 & X==1",
                 "Y==1 & X==1",
                 "Y==1 & X==1 & M==0",
                 "Y==1 & X==1 & M==1"),
    using = "parameters") 


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(data = data_function()) + 
  declare_inquiry(CoE = CoE) +
  declare_estimator(handler = my_estimator_function)


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


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------
# Difference-in-differences
# -------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
N_units <- 20
N_time_periods <- 20

design <-
  declare_population(
    units = add_level(
      N = N_units, 
      U_unit = rnorm(N), 
      D_unit = if_else(U_unit > median(U_unit), 1, 0),
      D_time = sample(1:N_time_periods, N, replace = TRUE)
    ),
    periods = add_level(
      N = N_time_periods,
      U_time = rnorm(N),
      nest = FALSE
    ),
    unit_period = cross_levels(
      by = join(units, periods), 
      U = rnorm(N),
      potential_outcomes(
        Y ~ U + U_unit + U_time + 
          D * (0.2 - 3 * (D_time - as.numeric(periods))), 
        conditions = list(D = c(0, 1))
      ),
      D = if_else(D_unit == 1 & as.numeric(periods) >= D_time, 1, 0),
      D_lag = lag_by_group(D, groups = units, n = 1, order_by = periods)
    )
  ) + 
  declare_inquiry(
    ATT_switchers = mean(Y_D_1 - Y_D_0), 
    subset = D == 1 & D_lag == 0 & !is.na(D_lag)
  ) + 
  declare_measurement(Y = reveal_outcomes(Y ~ D)) + 
  declare_estimator(
    Y ~ D, fixed_effects = ~ units + periods,
    model = lm_robust,
    inquiry = "ATT_switchers",
    label = "2wfe"
  ) + 
  declare_estimator(
    Y = "Y", 
    G = "units", 
    T = "periods", 
    D = "D",
    handler = label_estimator(did_multiplegt_tidy),
    inquiry = "ATT_switchers",
    label = "chaisemartin"
  ) 


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------
# Difference-in-differences :: Treatment timing
# ---------------------------------------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
declare_model(
  D_sametime = 
    if_else(D_unit == 1 & as.numeric(periods) >= N_time_periods/2, 1, 0)
  
  D_staggered = 
    if_else(D_unit == 1 & as.numeric(periods) >= D_time, 1, 0)
  
  D_one_period = 
    if_else(D_unit == 1 & as.numeric(periods) == D_time, 1, 0)
)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------
# Difference-in-differences :: Treatment effect heterogeneity
# -----------------------------------------------------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
declare_model(
  potential_outcomes(
    Y_homogenous ~ U + U_unit + U_time + D * 0.2,
    conditions = list(D = c(0, 1))
  ),
  potential_outcomes(
    Y_later_lower ~ U + U_unit + U_time + 
      D * (0.2 + 0.5 * (D_time - as.numeric(periods))),
    conditions = list(D = c(0, 1))
  )
  potential_outcomes(
    Y_later_higher ~ U + U_unit + U_time + 
      D * (0.2 - 0.5 * (D_time - as.numeric(periods))),
    conditions = list(D = c(0, 1))
  )
)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------
# Instrumental variables
# ----------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 100, 
    U = rnorm(N),
    potential_outcomes(D ~ if_else(Z + U > 0, 1, 0), 
                       conditions = list(Z = c(0, 1))), 
    potential_outcomes(Y ~ 0.1 * D + 0.25 + U, 
                       conditions = list(D = c(0, 1))),
    complier = D_Z_1 == 1 & D_Z_0 == 0
  ) + 
  declare_inquiry(LATE = mean(Y_D_1 - Y_D_0), subset = complier == TRUE) + 
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(D = reveal_outcomes(D, Z),
                      Y = reveal_outcomes(Y, D)) + 
  declare_estimator(Y ~ D | Z, model = iv_robust, inquiry = "LATE") 


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------
# Regression discontinuity designs
# --------------------------------

## ----rddeclaration-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cutoff <- 0.5
control <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}

design <-
  declare_model(
    N = 100,
    U = rnorm(N, 0, 0.1),
    X = runif(N, 0, 1) + U - cutoff,
    D = 1 * (X > 0),
    Y_D_0 = control(X) + U,
    Y_D_1 = treatment(X) + U
  ) +
  declare_inquiry(LATE = treatment(0.5) - control(0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ D)) + 
  declare_estimator(
    Y, X, c = 0, vce = "hc2",
    term = "Bias-Corrected",
    handler = label_estimator(rdrobust_helper),
    inquiry = "LATE",
    label = "optimal"
  )

