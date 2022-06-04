library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_17.8.R")
diagnosis_17.8 <-
  declaration_17.8 %>% 
  redesign(N = seq(500, 3000, 500)) %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

