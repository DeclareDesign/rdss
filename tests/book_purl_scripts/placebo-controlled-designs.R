library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# ------------------------------
# Placebo-controlled experiments
# ------------------------------

## ----file = "scripts_declarations/declaration_17.11a.R"-------------------------------------------------



## ----file = "scripts_declarations/declaration_17.11b.R"-------------------------------------------------


## ----file = "scripts_declarations/declaration_17.11c.R"-------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_17.11a <- 
  encouragement_design %>% 
  redesign(compliance_rate = seq(0.1, 0.9, by = 0.1)) %>% 
  diagnose_designs()

diagnosis_17.11b <- 
  placebo_controlled_design %>% 
  redesign(compliance_rate = seq(0.1, 0.9, by = 0.1)) %>% 
  diagnose_designs()


