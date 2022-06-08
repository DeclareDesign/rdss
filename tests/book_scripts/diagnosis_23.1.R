print('diagnosis_23.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_23.1.R")
diagnosands <- declare_diagnosands(
  power = mean(p.value <= 0.05),
  proportion_choose_control = mean(estimate < 0 | p.value > 0.05),
  proportion_choose_treatment = mean(estimate > 0 & p.value <= 0.05)
)
diagnosis_23.1 <-
  declaration_23.1 %>% 
  redesign(
    N = seq(from = 100, to = 3000, by = 500),
    effect_size = seq(from = 0, to = 0.25, by = 0.05)
  ) %>% 
  diagnose_designs(diagnosands = diagnosands, sims = sims, bootstrap_sims = bootstrap_sims)

