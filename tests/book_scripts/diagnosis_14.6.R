library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_14.6.R")
diagnosis_14.6 <- 
  declaration_14.6 %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

