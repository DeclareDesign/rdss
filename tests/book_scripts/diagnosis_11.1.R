library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_11.1.R")
diagnosis_11.1 <- 
  declaration_11.1 %>%
  redesign(N = seq(100, 1000, 100)) %>%
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

