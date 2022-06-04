library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------
# Declaration in code
# -------------------

## ----file = "scripts_declarations/declaration_12.1a.R"--------------------------------------------------


## ----file = "scripts_declarations/declaration_12.1b.R"--------------------------------------------------


## ----file = "scripts_declarations/declaration_12.1c.R"--------------------------------------------------


## ----file = "scripts_declarations/declaration_12.1d.R"--------------------------------------------------


## -------------------------------------------------------------------------------------------------------
declaration_12.1 <- model_12.1 + inquiry_12.1 + data_strategy_12.1 + answer_strategy_12.1


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------
# Diagnosis
# ---------

## -------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05),
  cost = mean(10 * n_villages + 1 * n_villages * citizens_per_village)
)


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_12.1 <- diagnose_design(declaration_12.1, diagnosands = diagnosands)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# --------
# Redesign
# --------

## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_12.2 <-
  declaration_12.1 %>%
  redesign(n_villages = c(192, 500),
           citizens_per_village = c(25, 50, 75, 100)) %>%
  diagnose_designs(diagnosands = diagnosands)


