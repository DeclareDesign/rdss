library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# --------------
# Model :: Units
# --------------

## ----results = "hide"-----------------------------------------------------------------------------------
M <- declare_model(N = 1000)


## ----results = "hide"-----------------------------------------------------------------------------------
M <- declare_model(
  households = add_level(
    N = 100, 
    N_members = sample(c(1, 2, 3, 4), N, 
                       prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
  ),
  individuals = add_level(
    N = N_members, 
    age = sample(18:90, N, replace = TRUE)
  )
)


## ----results = "hide"-----------------------------------------------------------------------------------
M <- declare_model(
  countries = add_level(
    N = 196, 
    country_shock = rnorm(N)
  ),
  years = add_level(
    N = 100, 
    time_trend = 1:N,
    year_shock = runif(N, 1, 10), 
    nest = FALSE
  ),
  observation = cross_levels(
    by = join_using(countries, years),
    observation_shock = rnorm(N),
    Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
  )
)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -----------------------------
# Model :: Unit characteristics
# -----------------------------

## ----results = "hide"-----------------------------------------------------------------------------------
M <- 
  declare_model(
    N = 100, 
    X = runif(N, min = 0, max = 100)
  )


## -------------------------------------------------------------------------------------------------------
M <-
  declare_model(
    N = 1000,
    X1 = rnorm(N, mean = 5, sd = 2),
    X2 = runif(N, min = 0, max = 5),
    X3 = rbinom(N, size = 1, prob = 0.5),
    X4 = rbinom(N, size = 5, prob = 0.5),
    X5 = rlnorm(N, meanlog = 0, sdlog = 1),
    X6 = sample(c(1, 2, 3, 4, 5), N, replace = TRUE)
  ) 


## -------------------------------------------------------------------------------------------------------
M1 <- 
  declare_model(
    N = 1000, 
    Y = rbinom(N, 1, prob = 0.5)
  )


## -------------------------------------------------------------------------------------------------------
M2 <- 
  declare_model(
    N = 1000, 
    latent = runif(N, min = 0, max = 1),
    Y = rbinom(N, 1, prob = latent),
    Y2 = latent + rnorm(N)
  )


## -------------------------------------------------------------------------------------------------------
M3 <- 
  declare_model(
    N = 1000, 
    latent = runif(N, min = 0, max = 1), 
    Y = if_else(latent > 0.75, 1, 0)
  )


## ----results = "hide"-----------------------------------------------------------------------------------
M1 <- 
  declare_model(
    N = 1000,
    X1 = rnorm(N),
    X2 = X1 + rnorm(N)
  )


## -------------------------------------------------------------------------------------------------------
M2 <-
  declare_model(
    draw_multivariate(c(X1, X2) ~ MASS::mvrnorm(
      N = 1000,
      mu = c(0, 0),
      Sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2)
    )))


## ----results = "hide"-----------------------------------------------------------------------------------
M <-
  declare_model(households = add_level(N = 1000),
                individuals = add_level(
                  N = 4,
                  X = draw_normal_icc(
                    mean = 0,
                    clusters = households,
                    ICC = 0.65
                  )
                ))


## ----results = "hide"-----------------------------------------------------------------------------------
M <- 
  declare_model(
    data = baseline_data,
    attitudes = sample(1:5, N, replace = TRUE)
  )


## ----eval = TRUE----------------------------------------------------------------------------------------
M <-
  declare_model(
    data = baseline_data, 
    N = 619505, 
    handler = resample_data
  )


## ----results = "hide"-----------------------------------------------------------------------------------
M1 <- declare_model(
  N = 100,
  U = rnorm(N),
  X = rbinom(N, size = 1, prob = 0.5),
  Y = 0.1 * X + U
)


## ----results = "hide"-----------------------------------------------------------------------------------
M2 <- declare_model(
  N = 100,
  U = rnorm(N),
  X = rbinom(N, size = 1, prob = pnorm(U)),
  Y = 0.1 * X + U
)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------------------------
# Model :: Potential outcomes
# ---------------------------

## ----results = "hide"-----------------------------------------------------------------------------------
M <- 
  declare_model(N = 100, 
                Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
                Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
                )


## ----results = "hide"-----------------------------------------------------------------------------------
M <- 
  declare_model(N = 100, 
                potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
  )
M()


## ----modelposdraw2, results = "hide"--------------------------------------------------------------------
M <- 
  declare_model(
    N = 100, 
    potential_outcomes(
      Y ~ rbinom(N, 1, prob = 0.1 * (Z == 1) + 0.2 * (Z == 2)), 
      conditions = list(Z = c(0, 1, 2))
    )
  )
M()


## ----modelposdraw3, results = "hide"--------------------------------------------------------------------
M <- 
  declare_model(
    N = 100, 
    potential_outcomes(
      Y ~ rbinom(N, 1, prob = 0.1 * Z1 + 0.2 * Z2 + 0.1 * Z1 * Z2), 
      conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))
    )
  )
M()


## -------------------------------------------------------------------------------------------------------
M <-
  declare_model(
    N = 100, 
    tau = runif(1, min = 0, max = 1), 
    U = rnorm(N), 
    potential_outcomes(Y ~ tau * Z + U)
  )


## ----results = "hide"-----------------------------------------------------------------------------------
M <- 
  declare_model(
    N = 100, 
    U = rnorm(N), 
    X = rbinom(N, 1, prob = 0.5),
    potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
  )


## ----results = "hide"-----------------------------------------------------------------------------------
M1 <- 
  declare_model(
    N = 100, 
    potential_outcomes(Y ~ rbinom(N, 1, prob = 0.2))
  )

M2 <- 
  declare_model(
    N = 100,
    latent = rnorm(N), 
    potential_outcomes(Y ~ rbinom(N, 1, prob = pnorm(latent + 0.2 * Z)))
  )

M3 <- 
  declare_model(
    N = 100, 
    latent = rnorm(N), 
    potential_outcomes(Y ~ if_else(latent + 0.2 * Z > 0.5, 1, 0))
  )


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------
# Inquiry
# -------

## ----results = "hide"-----------------------------------------------------------------------------------
M <- declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ Z + U))
I <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))


## ----results = "hide"-----------------------------------------------------------------------------------
M <- declare_model(N = 100, Y = rnorm(N))
I <- declare_inquiry(mean_Y = mean(Y))


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------
# Inquiry :: Inquiries among subsets of units
# -------------------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------
M <- declare_model(
  N = 100, 
    U = rnorm(N), 
    X = rbinom(N, 1, prob = 0.5),
    potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U))
I <- declare_inquiry(CATE = mean(Y_Z_1 - Y_Z_0), subset = X == 1)


## ----eval = TRUE----------------------------------------------------------------------------------------
I <- declare_inquiry(CATE = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]))


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------------------
# Inquiry :: Inquiries with continuous potential outcomes
# -------------------------------------------------------

## ----results = "hide"-----------------------------------------------------------------------------------
cutoff <- 0.5
control <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(0.7, -0.8, 0.5, 1.0))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(0.0, -1.5, 0.5, 0.8)) + 0.15}

I <- declare_inquiry(LATE = treatment(cutoff) - control(cutoff))


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -----------------------------
# Inquiry :: Multiple inquiries
# -----------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------
I <- declare_inquiry(
  ATE = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]),
  CATE_X0 = mean(Y_Z_1[X == 0] - Y_Z_0[X == 0]),
  CATE_X1 = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]),
  Difference_in_CATEs = CATE_X1 - CATE_X0)


## ----results = "hide"-----------------------------------------------------------------------------------
M <- 
  declare_model(
    counties = add_level(N = 5, county_quality_mean = rnorm(N)),
    schools = add_level(N = 5, school_quality = rnorm(N, mean = county_quality_mean))
  )

MRP_inquiry <- 
  function(data) {
    data %>% 
      group_by(counties) %>% 
      summarize(mean_school_quality = mean(school_quality),
                .groups = "drop")
  }

I <- declare_inquiry(handler = MRP_inquiry)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------
# Data strategy :: Sampling
# -------------------------

## -------------------------------------------------------------------------------------------------------
D <- declare_sampling(S = complete_rs(N = 100, n = 10))


## ----eval = TRUE----------------------------------------------------------------------------------------
D <-
  declare_sampling(
    # sample 20 districts
    S_districts = cluster_rs(clusters = districts, n = 20),
    # within each district, sample 50 villages
    S_villages  = strata_and_cluster_rs(
      strata = districts,
      clusters = villages,
      strata_n = 10
    ),
    # within each village select 25 households
    S_households  = strata_and_cluster_rs(
      strata = villages,
      clusters = households,
      strata_n = 25
    ),
    S = S_districts == 1 & S_villages == 1 & S_households == 1,
    filter = S == 1
  )


## ----eval = TRUE----------------------------------------------------------------------------------------
D <-
  declare_sampling(S = cluster_rs(clusters = districts, n = 20)) +
  declare_sampling(S = strata_and_cluster_rs(
    strata = districts,
    clusters = villages,
    strata_n = 10
  )) +
  declare_sampling(S = strata_and_cluster_rs(
    strata = villages,
    clusters = households,
    strata_n = 25
  ))


## -------------------------------------------------------------------------------------------------------
M <-
  declare_model(N = 100,
                X = rbinom(N, 1, prob = 0.5))

D <-
  declare_sampling(
    S = strata_rs(strata = X, strata_prob = c(0.2, 0.5)),
    S_inclusion_probability =
      strata_rs_probabilities(strata = X,
                              strata_prob = c(0.2, 0.5))
  )


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------
# Data strategy :: Treatment assignment
# -------------------------------------

## -------------------------------------------------------------------------------------------------------
D <- 
  declare_assignment(
    Z = complete_ra(N, m = 50),
    Z_condition_probability = 
      obtain_condition_probabilities(assignment = Z, m = 50)
  )


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------------------
# Data strategy :: Measurement
# ----------------------------

## -------------------------------------------------------------------------------------------------------
M <- declare_model(N = 100, latent = runif(N))
D <- declare_measurement(observed = rbinom(N, 1, prob = latent))


## -------------------------------------------------------------------------------------------------------
M <-
  declare_model(N = 100,
                potential_outcomes(Y ~ rbinom(
                  N, size = 1, prob = 0.1 * Z + 0.5
                )))

D <-
  declare_assignment(Z = complete_ra(N, m = 50)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))


## ----results = "hide"-----------------------------------------------------------------------------------
library(psych)

D <- declare_measurement(
  index = fa(
    r = cbind(Y_1, Y_2, Y_3),
    nfactors = 1,
    rotate = "varimax"
  )$scores
)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------------
# Answer strategy :: Statistical modeling functions
# -------------------------------------------------

## ----results = "hide"-----------------------------------------------------------------------------------
design <- 
  declare_model(
    N = 100, U = rnorm(N), potential_outcomes(Y ~ 0.2 * Z + U)
  ) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_estimator(model = lm_robust, 
                    formula = Y ~ Z, 
                    model_summary = tidy, 
                    term = "Z", 
                    inquiry = "ATE", 
                    label = "lm_no_controls")


## ----results = "hide"-----------------------------------------------------------------------------------
draw_estimates(design)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------------------------------------------------------
# Answer strategy :: Tidying statistical modelling function output
# ----------------------------------------------------------------

## ----results = "hide"-----------------------------------------------------------------------------------
A <- declare_estimator(Y ~ Z, 
                       model = lm_robust, 
                       model_summary = glance)
A(draw_data(design))


## ----eval = TRUE----------------------------------------------------------------------------------------
tidy_lm <- function(fit) {
  # calculate estimates by grabbing the coefficients from the model fit
  estimate <- coef(lm)
  
  # get the names of the coefficients (e.g., "(Intercept)", "Z")
  #   we will call these "term" to represent regression terms
  term <- names(estimates)
  
  # calculate the standard error by grabbing the variance-covariance
  #   matrix, then pulling the diagonal elements of it and taking the 
  #   square root to transform from variances to standard errors
  std.error <- sqrt(diag(vcov(lm)))
  
  # return a tibble with term, estimate, and std.error
  tibble(term = term, estimate = unlist(estimate), std.error = std.error)
}

declare_estimator(
  Y ~ Z,
  model = lm,
  model_summary = tidy_lm
)


## ----eval = TRUE----------------------------------------------------------------------------------------
tidy_margins <- function(x) {
  tidy(margins(x, data = x$data), conf.int = TRUE)
}

declare_estimator(
  Y ~ Z + X,
  model = glm,
  family = binomial("logit"),
  model_summary = tidy_margins,
  term = "Z"
) 


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------
# Answer strategy :: Custom answer strategies
# -------------------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------
my_estimator <- function(data) {
  data.frame(estimate = mean(data$Y))
}
declare_estimator(handler = label_estimator(my_estimator),
                  label = "mean",
                  inquiry = "Y_bar")


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -----------
# Declaration
# -----------

## ----file = "scripts_declarations/declaration_10.4.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
model +
  declare_inquiry(PATE = mean(Y_Z_1 - Y_Z_0)) +
  sampling +
  assignment +
  measurement +
  answer_strategy


## ----eval = TRUE----------------------------------------------------------------------------------------
model +
  sampling +
  declare_inquiry(SATE = mean(Y_Z_1 - Y_Z_0)) +
  assignment +
  measurement + 
  answer_strategy


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------
# Diagnosis
# ---------

## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_10.6 <-
  diagnose_design(declaration_10.4,
                  sims = 500,
                  bootstrap_sims = 100)


## ----eval = TRUE----------------------------------------------------------------------------------------
tidy(diagnosis_10.6)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------------------------
# Diagnosis :: Working directly with the simulations data frame
# -------------------------------------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------
simulations_df <- get_simulations(diagnosis_10.6)


## ----eval = TRUE----------------------------------------------------------------------------------------
simulations_df %>% 
  group_by(design, inquiry, estimator, term) %>% 
  summarize(
    bias = mean(estimate - estimand),
    rmse = sqrt(mean((estimate - estimand)^2)),
    power = mean(p.value <= 0.05),
    coverage = mean(estimand <= conf.high & estimand >= conf.low),
    .groups = "drop"
  )


## ----samplingdistincode, eval = TRUE--------------------------------------------------------------------
# first create summary for vertical lines
summary_df <- 
  simulations_df %>% 
  group_by(estimator) %>% 
  summarize(estimand = mean(estimand))

# then plot simulations
ggplot(simulations_df) +
  geom_histogram(aes(estimate),
                 bins = 40, fill = "#72B4F3") +
  geom_vline(data = summary_df,
             aes(xintercept = estimand),
             lty = "dashed", color = "#C6227F") +
  annotate("text", y = 80, x = 0, label = "Estimand",
           color = "#C6227F", hjust = 1) +
  facet_wrap(~ estimator) +
  labs(x = "Estimate", y = "Count of simulations") +
  theme_minimal()


## ----powercurveincode, eval = TRUE----------------------------------------------------------------------
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
  diagnose_design(design) %>% 
  get_simulations() %>% 
  mutate(significant = if_else(p.value <= 0.05, 1, 0))

ggplot(simulations_df) + 
  stat_smooth(aes(estimand, significant), method = 'loess', color = "#3564ED", fill = "#72B4F3", formula = 'y ~ x') +
  geom_hline(yintercept = 0.8, color = "#C6227F", linetype = "dashed") +
  annotate("text", x = 0, y = 0.85, label = "Conventional power threshold = 0.8", hjust = 0, color = "#C6227F") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "none") +
  labs(x = "Model parameter: true effect size",
       y = "Diagnosand: statistical power") +
  theme_minimal()


## ---- echo = TRUE---------------------------------------------------------------------------------------
# --------
# Redesign
# --------

## ----eval = TRUE----------------------------------------------------------------------------------------
designs <- list(design1, design2)


## ----eval = TRUE----------------------------------------------------------------------------------------
designs <- redesign(design, N = c(100, 200, 300))


## ----eval = TRUE----------------------------------------------------------------------------------------
designs <- diagnose_designs(designs)


