library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------
# Stepped-wedge experiments
# -------------------------

## ----file = "scripts_declarations/declaration_17.12.R"--------------------------------------------------


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------------------------------------
# Stepped-wedge experiments :: When to use a stepped wedge experiment
# -------------------------------------------------------------------

## ----file = "scripts_declarations/declaration_17.13.R"--------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
design_stepped_wedge <- 
  declaration_17.12 %>% 
  redesign(n_units = 100, effect_size = seq(from = 0, to = 0.75, by = 0.05))

design_single_period_100 <- 
  declaration_17.13 %>% 
  redesign(n_units = 100, effect_size = seq(from = 0, to = 0.75, by = 0.05))
  
design_single_period_200 <-
  declaration_17.13 %>% 
  redesign(n_units = 200, effect_size = seq(from = 0, to = 0.75, by = 0.05))

designs <- c(design_stepped_wedge, design_single_period_100, design_single_period_200)
attr(designs, "names") <- paste0("design_", 1:length(designs))

diagnosis_17.12 <- diagnose_design(designs)


