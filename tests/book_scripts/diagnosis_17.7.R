library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_17.7.R")
diagnosis_17.7 <- 
  declaration_17.7 %>% 
  redesign(n_x1 = seq(20, 980, by = 96)) %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

