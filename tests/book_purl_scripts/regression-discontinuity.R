library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# --------------------------------
# Regression discontinuity designs
# --------------------------------

## ----file = "scripts_declarations/declaration_15.5.R"---------------------------------------------------


## ----file = "scripts_declarations/declaration_15.6.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_15.5 <- 
  declaration_15.6 %>% 
  redesign(bandwidth = seq(from = 0.05, to = 0.5, by = 0.05)) %>% 
  diagnose_designs


