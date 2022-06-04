library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_17.14 <-
  declare_model(
    units = add_level(
      N = 100, 
      U_unit = rnorm(N, sd = 5)
    ),
    periods = add_level(
      N = 2, 
      time = 1:2, 
      U_time = rnorm(N), 
      nest = FALSE
    ),
    unit_periods = cross_levels(
      by = join_using(units, periods), 
      U = rnorm(N),
      Y_Z_0_Zlag_0 = U_time + U_unit + U,
      Y_Z_1_Zlag_0 = Y_Z_0_Zlag_0 + 0.2,
      Y_Z_0_Zlag_1 = Y_Z_0_Zlag_0 + 0.2 * carryover
    )
  ) +
  declare_inquiry(
    ATE_untreated_before = mean(Y_Z_1_Zlag_0 - Y_Z_0_Zlag_0)
  ) + 
  declare_assignment(
    Z = block_ra(blocks = units, prob = 0.5),
    Zlag = if_else(time == 2 & Z == 0, 1, 0)
  ) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z + Zlag)) +
  declare_estimator(
    Y ~ Z, 
    cluster = units,
    fixed_effects = ~units,
    se_type = "stata",
    model = lm_robust, 
    inquiry = "ATE_untreated_before"
  ) 
