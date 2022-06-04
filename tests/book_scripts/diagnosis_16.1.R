library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_16.2.R")
diagnosis_16.1 <- 
  declaration_16.2 %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

