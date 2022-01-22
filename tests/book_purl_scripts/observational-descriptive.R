## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------
# Simple random sampling
# ----------------------

## ----tmpasdf-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(343)
portola <-
  fabricate(
    N = 2100,
    Y_star = rnorm(N)
    )

design <- 
  declare_model(data = portola) + 
  declare_measurement(Y = as.numeric(cut(Y_star, 7))) + 
  declare_inquiry(Y_bar = mean(Y)) + 
  declare_sampling(S = complete_rs(N, n = 100)) + 
  declare_estimator(Y ~ 1, inquiry = "Y_bar")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2))
)
diagnosis <- diagnose_design(design, diagnosands = diagnosands) 


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------
# Simple random sampling :: What can go wrong
# -------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(data = portola) + 
  declare_measurement(Y = as.numeric(cut(Y_star, 7))) + 
  declare_inquiry(Y_bar = mean(Y)) + 
  declare_sampling(S = complete_rs(N, n = 100)) + 
  declare_measurement(
    R = rbinom(n = N, size = 1, prob = pnorm(Y_star + effort)),
    Y = if_else(R == 1, Y, NA_real_)
  ) +
  declare_estimator(Y ~ 1, inquiry = "Y_bar") +
  declare_estimator(R ~ 1, label = "Response Rate")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, effort = seq(0, 5, by = 0.5))
diagnosis <- diagnose_designs(designs, sims = 500)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------
# Cluster random sampling
# -----------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(343)
ICC <- 0.4

two_nigerian_states <-
  fabricate(
    state = add_level(N = 2, 
                      state_name = c("taraba", "kwara"),
                      state_mean = c(-0.2, 0.2)),
    locality = add_level(
      N = 500,
      locality_shock = rnorm(N, state_mean, sqrt(ICC))
    ),
    individual = add_level(
      N = 100,
      individual_shock = rnorm(N, sd = sqrt(1 - ICC)),
      Y_star = locality_shock + individual_shock
    )
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
budget_function <- 
  function(cluster_prob){
  budget = 20000
  cluster_cost = 20
  individual_cost = 2
  n_clusters = 1000
  n_individuals_per_cluster = 100
  
  total_cluster_cost <-
    cluster_prob * n_clusters * cluster_cost
  
  remaining_funds <- budget - total_cluster_cost
  
  sampleable_individuals <- 
    cluster_prob * n_clusters * n_individuals_per_cluster
  
  individual_prob = 
    (remaining_funds/individual_cost)/sampleable_individuals
  
  pmin(individual_prob, 1)
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(data = two_nigerian_states) +
  declare_measurement(Y = as.numeric(cut(Y_star, 7))) +
  declare_inquiry(Y_bar = mean(Y)) +
  declare_sampling(
    S_cluster = strata_and_cluster_rs(
      strata = state,
      clusters = locality,
      prob = cluster_prob
    ),
    filter = S_cluster == 1
  ) +
  declare_sampling(
    S_individual = 
      strata_rs(strata = locality, 
                prob = budget_function(cluster_prob)),
    filter = S_individual == 1
  ) +
  declare_estimator(Y ~ 1,
                    clusters = locality,
                    inquiry = "Y_bar")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, cluster_prob = seq(0.1, 0.9, 0.1))
diagnosis <- diagnose_design(designs, sims = 500)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------
# Multi-level regression and poststratification
# ---------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
states <- 
  as_tibble(state.x77) %>%
  transmute(
    state = rownames(state.x77),
    prop_of_US = Population / sum(Population),
    prob_HS = `HS Grad` / 100,
    state_shock = rnorm(n = n(), sd = 0.5),
    state_mean = prob_HS * pnorm(0.2 + state_shock) + (1 - prob_HS) * pnorm(state_shock)
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    data = states[sample(1:50,
                         size = 2000,
                         replace = TRUE,
                         prob = states$prop_of_US),],
    HS = rbinom(n = N, size = 1, prob = prob_HS),
    PS_weight =
      case_when(HS == 0 ~ (1 - prob_HS),
                HS == 1 ~ prob_HS),
    individual_shock = rnorm(n = N, sd = 0.5),
    policy_support = rbinom(N, 1, prob = pnorm(0.2 * HS + individual_shock + state_shock))
  ) +
  declare_inquiry(
    handler = function(data) {
      states %>% transmute(state, inquiry = "mean_policy_support",  estimand = state_mean)
    }
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ps_helper <- function(model_fit, data) {
  prediction(
    model_fit,
    data = data,
    allow.new.levels = TRUE,
    type = "response"
  ) %>%
    group_by(state) %>%
    summarize(estimate = weighted.mean(fitted, PS_weight))
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  design +
  declare_estimator(handler = label_estimator(function(data) {
    model_fit <- glmer(
      formula = policy_support ~ HS + (1 | state),
      data = data,
      family = binomial(link = "logit")
    )
    ps_helper(model_fit, data = data)
  }),
  label = "Partial pooling",
  inquiry = "mean_policy_support")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# Multi-level regression and poststratification :: Redesign over answer strategies
# --------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  design +
  
  declare_estimator(
    handler = label_estimator(function(data) {
      model_fit <- lm_robust(
        formula = policy_support ~ HS + state,
        data = data
      )
      ps_helper(model_fit, data = data)
    }),
    label = "No pooling",
    inquiry = "mean_policy_support") +
  
  declare_estimator(
    handler = label_estimator(function(data) {
      model_fit <- lm_robust(
        formula = policy_support ~ HS,
        data = data
      )
      ps_helper(model_fit, data = data)
    }),
    label = "Full pooling",
    inquiry = "mean_policy_support")



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


