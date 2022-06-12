print('declaration_19.4_NEW.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(metafor)
# Idiosyncratic features of studies
study_sizes <- c(500, 1000, 1500, 2000, 2500)
study_assignment_probabilities <- c(.5, .5, .6, .7, .8)
study_intercepts <- 1:5
study_priors <- seq(0, .3, length = 5)
study_coordination <- TRUE
# helper function to estimate study level effects
estimate_study_effects <-
  function(formula, data) {
    data |>
      group_by(sites) |>
      do(tidy(lm_robust(formula, data = .))) |>
      filter(term == "Z") |>
      ungroup()
  }
declaration_19.4 <-
  declare_model(
    sites = add_level(N = 5,
                      size = study_sizes,
                      intercept = study_intercepts,
                      tau_1 = rnorm(N, study_priors, 0.1),
                      tau_2 = rnorm(N, 0.3, 0.2),
                      coordinated = study_coordination,
                      # if coordinated, take tau_1,
                      # otherwise take the bigger of the two
                      tau = if_else(coordinated,
                                    tau_1,
                                    pmax(tau_1, tau_2))),
    subjects = add_level(N = size)) +
  declare_model(potential_outcomes(Y ~ intercept + tau * Z + rnorm(N))) +
  declare_inquiry(
    # Sample and trial level ATEs
    Actual_SATE = mean(Y_Z_1 - Y_Z_0),
    Actual_TATE = weighted.mean(Y_Z_1 - Y_Z_0, 1/size),
    Target_TATE = weighted.mean(tau_1, 1/size)) +
  declare_assignment(Z = block_ra(blocks  = sites,
                                  block_prob = study_assignment_probabilities)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_step(Y ~ Z, handler = estimate_study_effects) +
  declare_estimator(
    yi = estimate,
    sei = std.error,
    method = "REML",
    .method = rma_helper,
    .summary = rma_mu_tau,
    term = "mu",
    inquiry = c("Actual_SATE", "Actual_TATE", "Target_TATE")
  )
design_coordinated <- redesign(declaration_19.4, study_coordination = TRUE)
design_uncoordinated <- redesign(declaration_19.4, study_coordination = FALSE)
