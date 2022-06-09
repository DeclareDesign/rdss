print('diagnosis_15.4.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
library(DIDmultiplegt)
sims <- 3
bootstrap_sims <- 3
lag_by_group <- function(x, groups, n = 1, order_by, default = NA) {
  x_nm <- enexpr(x)
  grp_nm <- enexpr(groups)
  tibble(!!x_nm := x, !!grp_nm := groups, order_by___ = order_by) %>% 
    group_by(!!grp_nm) %>% 
    mutate(!!x_nm := lag(!!x_nm, n = n, default = NA, order_by = order_by___)) %>% 
    ungroup %>% 
    pull(!!x_nm)
}
did_multiplegt_tidy <- function(data, ...) {
  fit <- did_multiplegt(df = data, ...)
  tibble(estimate = fit$effect)
}
N_units <- 20
N_time_periods <- 20
M_units <- declare_model(
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
    by = join_using(units, periods), 
    U = rnorm(N)
  )
) 
M_PO_homogenous <-
  declare_model(
    potential_outcomes(Y ~ U + U_unit + U_time + D * 0.2, conditions = list(D = c(0, 1)))
  )
M_PO_later_lower <-
  declare_model(
    potential_outcomes(Y ~ U + U_unit + U_time + D * (0.2 + 1 * (D_time - as.numeric(periods))), conditions = list(D = c(0, 1)))
  )
M_PO_later_higher <- 
  declare_model(
    potential_outcomes(Y ~ U + U_unit + U_time + D * (0.2 - 1 * (D_time - as.numeric(periods))), conditions = list(D = c(0, 1)))
  )
M_D_staggered <- 
  declare_model(
    D = if_else(D_unit == 1 & as.numeric(periods) >= D_time, 1, 0),
    D_lag = lag_by_group(D, groups = units, n = 1, order_by = periods)
  )
I <-   
  declare_inquiry(
    ATT = mean(Y_D_1 - Y_D_0), 
    subset = D == 1
  ) + 
  declare_inquiry(
    ATT_switchers = mean(Y_D_1 - Y_D_0), 
    subset = D == 1 & D_lag == 0 & !is.na(D_lag)
  ) 
D <- declare_measurement(Y = reveal_outcomes(Y ~ D))
A <- 
  declare_estimator(
    Y ~ D, fixed_effects = ~ units + periods,
    .method = lm_robust,
    inquiry = c("ATT", "ATT_switchers"),
    label = "twoway-fe"
  ) +
  declare_estimator(
    Y = "Y", 
    G = "units", 
    T = "periods", 
    D = "D",
    handler = label_estimator(did_multiplegt_tidy),
    inquiry = c("ATT", "ATT_switchers"),
    label = "chaisemartin"
  ) 
D_homogenous <- M_units + M_PO_homogenous + M_D_staggered + I + D + A
D_later_lower <- M_units + M_PO_later_lower + M_D_staggered + I + D + A
D_later_higher <- M_units + M_PO_later_higher + M_D_staggered + I + D + A
diagnosis_15.4 <- 
  diagnose_design(PO_homogenous = D_homogenous, 
                  PO_later_lower = D_later_lower, 
                  PO_later_higher = D_later_higher, 
                  sims = sims, bootstrap_sims = bootstrap_sims)

