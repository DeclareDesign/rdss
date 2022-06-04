## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ------------------------------
# Discovery using causal forests
# ------------------------------

## ----file = "scripts_declarations/declaration_18.2.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
most_common <- 
  function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

discovery_diagnosands  <- 
  declare_diagnosands(
    correct = mean(estimate == estimand),
    bias = mean(estimate - estimand),
    mean_estimate = mean(estimate),
    modal_estimate = most_common(round(estimate, 1)),
    mean_estimand = mean(estimand),
    modal_estimand = most_common(round(estimand, 1))
  )

diagnosis_18.2 <- diagnose_design(declaration_18.2, diagnosands = discovery_diagnosands) 


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ---------------------
# Structural estimation
# ---------------------

## ----file = "scripts_declarations/declaration_18.3a.R"--------------------------------------------------------------------------------


## ----file = "scripts_declarations/declaration_18.3b.R"--------------------------------------------------------------------------------



## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosis_18.3 <- diagnose_design(declaration_18.3) 


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# -------------
# Meta-analysis
# -------------

## ----file = "scripts_declarations/declaration_18.4.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosis_18.4 <- diagnose_design(declaration_18.4)


## -------------------------------------------------------------------------------------------------------------------------------------
diagnosis_18.4 %>% 
  reshape_diagnosis(select = c("Bias", "RMSE", "Coverage")) %>% 
  kable(
    booktabs = TRUE,
    align = "c",
    digits = 2,
    caption = "Bias, RMSE, and coverage from the random effects and fixed effect estimators under the models assumed by the estimators."
  )


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ------------------
# Multi-site studies
# ------------------

## -------------------------------------------------------------------------------------------------------------------------------------
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
                    model = rma_helper, model_summary = rma_mu_tau, inquiry = "PATE", 
                    label = "random-effects")


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------
# Multi-site studies :: Coordinating treatments
# ---------------------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
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
    model = rma_helper,
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

