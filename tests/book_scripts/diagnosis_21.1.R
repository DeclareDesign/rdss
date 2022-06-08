print('diagnosis_21.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_21.x.R")
diagnosis_21.1 <- 
  declaration_21.x %>% 
  redesign(prob = seq(1 / 12, 11 / 12, length.out = 11)) %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

