## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ----------------
# List experiments
# ----------------

## ----file = "scripts_declarations/declaration_16.3.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  mean_CI_width = mean(conf.high - conf.low)
)
diagnosis_16.2 <- diagnose_design(declaration_16.3, diagnosands = diagnosands)


## ----file = "scripts_declarations/declaration_16.4.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosis_16.3 <- 
  declaration_16.4 %>% 
  redesign(proportion_hiding = seq(from = 0, to = 0.3, by = 0.1), 
           N = seq(from = 500, to = 2500, by = 500)) %>% 
  diagnose_design


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# ---------------------------------
# List experiments :: Exercises {-}
# ---------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
library(rdddr)
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

