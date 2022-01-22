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


