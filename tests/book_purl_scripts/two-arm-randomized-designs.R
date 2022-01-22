## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------
# Two-arm randomized experiments :: Design diagnosis through simulation
# ---------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
eq_3.4_designer <-
  function(N, m, var_Y0, var_Y1, cov_Y0_Y1, mean_Y0, mean_Y1) {
    
    fixed_sample <-
      MASS::mvrnorm(
        n = N,
        mu = c(mean_Y0, mean_Y1),
        Sigma = matrix(c(var_Y0, cov_Y0_Y1, cov_Y0_Y1, var_Y1), nrow = 2),
        empirical = TRUE # this line makes the means and variances "exact" in the sample data
      ) %>%
      magrittr::set_colnames(c("Y_Z_0", "Y_Z_1"))
    
    declare_model(data = fixed_sample) +
      declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
      declare_assignment(Z = complete_ra(N = N, m = m)) +
      declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
      declare_estimator(Y ~ Z, inquiry = "ATE")
    
  }


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- 
  expand_design(designer = eq_3.4_designer,
                N = 100,
                m = seq(10, 90, 10),
                var_Y0 = 1,
                var_Y1 = 2,
                cov_Y0_Y1 = 0.5,
                mean_Y0 = 1.0,
                mean_Y1 = 1.75)

dx <- diagnose_designs(designs, sims = 100, bootstrap_sims = FALSE)


