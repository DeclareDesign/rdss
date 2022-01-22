## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------
# List experiments
# ----------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(
    N = 5000,
    control_count = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ Y_star * Z + control_count) 
  ) +
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, model = difference_in_means, 
                    inquiry = "prevalence_rate")


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  mean_CI_width = mean(abs(conf.high - conf.low))
)
diagnosis <- diagnose_design(design, sims = sims, diagnosands = diagnosands)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------
# List experiments :: Assumption violations
# -----------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design_design_effects <- 
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count_control = rbinom(N, size = 3, prob = 0.5),
    control_count_treat = rbinom(N, size = 3, prob = 0.25),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ (Y_star + control_count_treat) * Z + control_count_control * (1 - Z))
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")


## ----eval = do_diagnosis-------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_design_effects <- diagnose_design(design_design_effects, sims = sims)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design_liars <- 
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(
      Y_list ~ 
        if_else(control_count == 3 & Y_star == 1 & Z == 1, 
                3, 
                Y_star * Z + control_count))
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")


## ----eval = do_diagnosis-------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_liars <- diagnose_design(design_liars, sims = sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------
# List experiments :: Choosing design parameters
# ----------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    W = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = proportion_shy)),
    potential_outcomes(Y_list ~ Y_star * Z + control_count)
  ) +
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = n)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z),
                      Y_direct = Y_star - W) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate", label = "list") + 
  declare_estimator(Y_direct ~ 1, inquiry = "prevalence_rate", label = "direct")

designs <- redesign(design, proportion_shy = seq(from = 0, to = 0.3, by = 0.1), n = seq(from = 500, to = 2500, by = 500))


## ----eval = do_diagnosis-------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis_tradeoff <- diagnose_design(designs, sims = sims, bootstrap_sims = b_sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------
# List experiments :: Exercises {-}
# ---------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_rr <-
  declare_model(
    N = 100,
    U = rnorm(N),
    X = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(
      Y_rr ~
        case_when(
          dice == 1 ~ 0L,
          dice %in% 2:5 ~ Y_star,
          dice == 6 ~ 1L
        ),
      conditions = list(dice = 1:6))
  ) + 
  declare_assignment(
    dice = complete_ra(N, prob_each = rep(1/6, 6),
                       conditions = 1:6)) +
  declare_measurement(Y_rr = reveal_outcomes(Y_rr ~ dice)) + 
  declare_estimator(Y_rr ~ 1, handler = label_estimator(rr_forced_known),
                    label = "forced_known", inquiry = "proportion")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count = rbinom(N, size = J, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ Y_star * Z + control_count) 
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")

designs <- redesign(design, J = 2:5)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis_control_item_count <- diagnose_design(designs, sims = sims)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis_control_item_count %>%
  get_diagnosands %>%
  select(J, bias, rmse) %>%
  kable(booktabs = TRUE, align = "c", digits = 3,
        caption = "Redesign  over number of control items")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_item_1 = draw_binary(0.5, N), 
    control_item_2 = correlate(given = control_item_1, rho = rho, draw_binary, prob = 0.5),
    control_item_3 = draw_binary(0.5, N),
    control_count = control_item_1 + control_item_2 + control_item_3,
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ Y_star * Z + control_count)
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")

designs <- redesign(design, rho = seq(from = 0, to = 1, by = 0.25))


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_control_item_correlation <- diagnose_design(designs, sims = sims, bootstrap_sims = b_sims)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_control_item_correlation %>%
  get_diagnosands %>%
  select(rho, estimator, inquiry, bias, rmse) %>%
  kable(booktabs = TRUE, align = "c", digits = 3)

