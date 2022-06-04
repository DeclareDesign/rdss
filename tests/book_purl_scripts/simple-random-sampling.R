library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------------
# Simple random sampling
# ----------------------

## ----file = "scripts_declarations/declaration_14.1.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2))
)
diagnosis_14.1 <- diagnose_design(declaration_14.1, diagnosands = diagnosands) 


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------
# Simple random sampling :: What can go wrong
# -------------------------------------------

## ----file = "scripts_declarations/declaration_14.2.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_14.2 <- 
  declaration_14.2 %>% 
  redesign(effort = seq(0, 5, by = 0.5)) %>% 
  diagnose_designs()


