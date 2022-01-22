## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------
# Index creation
# --------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 500,
    X = rep(0:1, N / 2),
    Y_star = 1 + X + 2 * rnorm(N)
  ) +
  declare_inquiry(Y_bar_X1 = mean(scale(Y_star)[X == 1])) +
  declare_measurement(
    Y_1 = 3 + 0.1 * Y_star + rnorm(N, sd = 5),
    Y_2 = 2 + 1.0 * Y_star + rnorm(N, sd = 2),
    Y_3 = 1 + 0.5 * Y_star + rnorm(N, sd = 1),
    Y_av = ((scale(Y_1) + scale(Y_2) + scale(Y_3))),
    Y_av_adj = (
      # rescaling according to the X = 0 group
      (Y_1 - mean(Y_1[X == 0])) / sd(Y_1[X == 0]) +
        (Y_2 - mean(Y_2[X == 0])) / sd(Y_2[X == 0]) +
        (Y_3 - mean(Y_3[X == 0])) / sd(Y_3[X == 0])
    ) / 3,
    Y_av_scaled = scale((scale(Y_1) + scale(Y_2) + scale(Y_3))),
    Y_fa  = princomp( ~ Y_1 + Y_2 + Y_2, cor = TRUE)$scores[, 1]
  ) +
  declare_estimator(
    Y_av ~ 1,
    model = lm_robust,
    inquiry = "Y_bar_X1",
    subset = X == 1,
    label = "Average"
  ) +
  declare_estimator(
    Y_av_scaled ~ 1,
    model = lm_robust,
    inquiry = "Y_bar_X1",
    subset = X == 1,
    label = "Average (rescaled)"
  ) +
  declare_estimator(
    Y_av_adj ~ 1,
    model = lm_robust,
    inquiry = "Y_bar_X1",
    subset = X == 1,
    label = "Average (adjusted)"
  ) +
  declare_estimator(
    Y_fa ~ 1,
    model = lm_robust,
    inquiry = "Y_bar_X1",
    subset = X == 1,
    label = "Principal Components"
  )



## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- diagnose_design(design) 


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------
# Index creation :: Two factor models
# -----------------------------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rho <- 0.0

design <-
  declare_model(
    N = 1000,
    draw_multivariate(c(latent_economic, latent_social) ~ mvrnorm(
      n = N,
      mu = c(0, 0),
      Sigma = matrix(c(1, rho, rho, 1), 2, 2)
    ))

  ) +
  declare_inquiry(true_cor = cor(latent_economic, latent_social)) +
  declare_measurement(
    Y_1 = scale(0.1 * latent_economic + 0.9 * latent_social + rnorm(N, sd = 0.2)),
    Y_2 = scale(0.3 * latent_economic + 0.7 * latent_social + rnorm(N, sd = 0.2)),
    Y_3 = scale(0.5 * latent_economic + 0.5 * latent_social + rnorm(N, sd = 0.2)),
    Y_4 = scale(0.7 * latent_economic + 0.3 * latent_social + rnorm(N, sd = 0.2)),
    Y_5 = scale(0.9 * latent_economic + 0.1 * latent_social + rnorm(N, sd = 0.2))
  ) +
  declare_measurement(FA = fa(
    r = cbind(Y_1, Y_2, Y_3, Y_4, Y_5),
    nfactors = 2,
    rotate = "oblimin",
    scores = "tenBerge"
  )$scores) +
   declare_estimator(~FA.MR1 + FA.MR2, model = stats:::cor.test.formula, inquiry = "true_cor")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, rho = seq(-1, 1, length.out = 10))
simulations <- simulate_design(designs, sims = 100)


