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

