print('diagnosis_14.2.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_14.1.R")
source("../book_scripts/declaration_14.2.R")
diagnosis_14.2 <- 
  declaration_14.2 %>% 
  redesign(effort = seq(0, 5, by = 0.5)) %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

