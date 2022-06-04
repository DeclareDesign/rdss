library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# -----------------------
# Elements :: Uncertainty
# -----------------------

## ----eval = TRUE----------------------------------------------------------------------------------------
three_italian_citizens <- fabricate(N = 3, age = c(5, 15, 25))
answer_strategy <- declare_estimator(age ~ 1)
answer_strategy(three_italian_citizens)


## ----file = "scripts_declarations/declaration_9.5.R"----------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_9.5 <- diagnose_designs(declaration_9.5)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------------------------
# Lexicon :: Point estimation
# ---------------------------

## ----file = "scripts_declarations/declaration_9.1.R"----------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_9.1 <- diagnose_design(declaration_9.1)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------
# Lexicon :: Tests
# ----------------

## ----file = "scripts_declarations/declaration_9.2.R"----------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
run_design(declaration_9.2)


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_9.2 <- diagnose_design(declaration_9.2)


## ----results = "hide"-----------------------------------------------------------------------------------
observed_estimate <-
  difference_in_means(views ~ success, data = clingingsmith_etal)

observed_estimate


## ----file = "scripts_declarations/declaration_9.3.R"----------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
p.value <-
  declare_diagnosands(
    p.value = mean(abs(estimate) >= abs(observed_estimate$coefficients))
  )

diagnosis_9.3 <-
  diagnose_design(
    declaration_9.3,
    diagnosands = p.value
  )

tidy(diagnosis_9.3)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------------------------
# Lexicon :: Bayesian formalizations
# ----------------------------------

## ----file = "scripts_declarations/declaration_9.4.R"----------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_9.4 <- diagnose_design(declaration_9.4)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ------------------------------
# Lexicon :: Interval estimation
# ------------------------------

## ----results = "hide"-----------------------------------------------------------------------------------
lower_bound <- (3 * 15 + 97 * 0)/100
upper_bound <- (3 * 15 + 97 * 110)/100
 c(lower_bound, upper_bound)


## ----results = "hide"-----------------------------------------------------------------------------------
lower_bound <- (90 * 44 + 10 * 0)/100
upper_bound <- (90 * 44 + 10 * 110)/100
c(lower_bound, upper_bound)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------------
# How to choose :: Seek theory-empirics parallelism
# -------------------------------------------------

## ----file = "scripts_declarations/declaration_9.6.R"----------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_9.6 <- diagnose_design(declaration_9.6)


