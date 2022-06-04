library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------
# Subgroup designs
# ----------------

## ----file = "scripts_declarations/declaration_17.7.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_17.7 <- 
  declaration_17.7 %>% 
  redesign(n_x1 = seq(20, 980, by = 96)) %>% 
  diagnose_designs()


