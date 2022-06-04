library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_17.14.R")
diagnosis_17.13 <- 
  declaration_17.14 %>% 
  redesign(carryover = seq(from = 0, to = 1, by = 0.1)) %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

