## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------
# Estimating diagnosands via simulation
# -------------------------------------

## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
sims <- 500
p.values <- rep(NA, sims)

for(i in 1:sims){
  Z <- rbinom(100, 1, 0.5)
  U <- rnorm(100)
  Y <- 0.2 * Z + U
  p.values[i] <- summary(lm(Y ~ Z))$coefficients[2, 4]
}

power <- mean(p.values <= 0.05)
power


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 100,
    U = rnorm(N),
    potential_outcomes(Y ~  0.2 * Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(power = mean(p.value <= 0.05))

diagnosis <- 
  diagnose_design(design, 
                  diagnosands = diagnosands,
                  sims = 500)
diagnosis


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------
# Estimating diagnosands via simulation :: Breaking down diagnosis
# ----------------------------------------------------------------

## ----eval = TRUE, echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------
run_design(design)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- 
  diagnose_design(
    design, sims = 1000, 
    diagnosands = declare_diagnosands(
      bias = mean(estimate - estimand),
      true_se = sd(estimate),
      power = mean(p.value <= 0.05),
      coverage = mean(estimand <= conf.high & estimand >= conf.low)
    )
  )


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------
# Diagnosis under model uncertainty
# ---------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 200, U = rnorm(N),
    # this runif(1, 0, 0.5) generates 1 random ATE between 0 and 0.5
    potential_outcomes(Y ~ runif(1, 0, 0.5) * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Diagnosis under model uncertainty :: Adjudicating between competing models
# --------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
M1 <-
  declare_model(
    N = 200,
    U = rnorm(N),
    potential_outcomes(Y1 ~ 0.2 * Z + U),
    potential_outcomes(Y2 ~ 0.0 * Z + U)
  )

M2 <-
  declare_model(
    N = 200,
    U = rnorm(N),
    potential_outcomes(Y1 ~ 0.0 * Z + U),
    potential_outcomes(Y2 ~ 0.2 * Z + U)
  )

IDA <- 
  declare_inquiry(ATE1 = mean(Y1_Z_1 - Y1_Z_0),
                  ATE2 = mean(Y2_Z_1 - Y2_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y1 = reveal_outcomes(Y1 ~ Z),
                      Y2 = reveal_outcomes(Y2 ~ Z)) +
  declare_estimator(Y1 ~ Z, inquiry = "ATE1", label = "DIM1") +
  declare_estimator(Y2 ~ Z, inquiry = "ATE2", label = "DIM2")

design1 <- M1 + IDA
design2 <- M2 + IDA


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------
# Diagnosing a design in code
# ---------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
effect_size <- 0.1
design <-
  declare_model(
    N = 100,
    U = rnorm(N),
    X = rnorm(N),
    potential_outcomes(Y ~ effect_size * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "unadjusted") + 
  declare_estimator(Y ~ Z + X, inquiry = "ATE", label = "adjusted")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
simulations_df <- simulate_design(design, sims = 100)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
simulations_df %>% 
  group_by(design, inquiry, estimator, term) %>% 
  summarize(
    bias = mean(estimate - estimand),
    rmse = sqrt(mean((estimate - estimand)^2)),
    power = mean(p.value < 0.05),
    coverage = mean(estimand <= conf.high & estimand >= conf.low),
    mean_estimate = mean(estimate),
    sd_estimate = sd(estimate),
    mean_se = mean(std.error),
    type_s_rate = 
      mean((sign(estimate) != sign(estimand))[p.value < 0.05]),
    mean_estimand = mean(estimand),
    .groups = "drop"
  )


## ----samplingdistincode, fig.cap = "Example visualization of a diagnosis", fig.width = 6.5, fig.height = 3---------------------------------------------------------------------------------
# first create summary for vertical lines
summary_df <- 
  simulations_df %>% 
  group_by(estimator) %>% 
  summarize(estimand = mean(estimand))

# then plot simulations
ggplot(simulations_df) +
  geom_histogram(aes(estimate),
                 bins = 40, fill = "lightblue") +
  geom_vline(data = summary_df,
             aes(xintercept = estimand),
             lty = "dashed", color = "red") +
  annotate("text", y = 80, x = 0, label = "Estimand",
           color = "red", hjust = 1) +
  facet_wrap( ~ estimator) +
  labs(x = "Estimate", y = "Count of simulations")


## ----powercurveincode, fig.cap = "Example visualization of a diagnosis", fig.width = 6.5, fig.height = 2-----------------------------------------------------------------------------------
design <-
  declare_model(
    N = 200,
    U = rnorm(N),
    potential_outcomes(Y ~ runif(1, 0.0, 0.5) * Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE") 

simulations_df <- 
  simulate_designs(design, sims = 500) %>% 
  mutate(significant = if_else(p.value <= 0.05, 1, 0))

ggplot(simulations_df) + 
  stat_smooth(aes(estimand, significant), method = 'loess', color = "blue", fill = "lightblue", formula = 'y ~ x') +
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed") +
  annotate("text", x = 0, y = 0.85, label = "Conventional power threshold = 0.8", hjust = 0, color = "red") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "none") +
  labs(x = "Model parameter: true effect size",
       y = "Diagnosand: statistical power")

