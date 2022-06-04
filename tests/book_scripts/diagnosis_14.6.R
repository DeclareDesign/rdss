library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_14.6.R")
diagnosis_14.6 <- 
  declaration_14.6 %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

