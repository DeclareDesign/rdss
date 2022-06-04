library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_17.8.R")
diagnosis_17.9 <-
  declaration_17.8 %>% 
  redesign(
    CATE_Z1_Z2_0 = seq(0, 0.5, 0.05),
    CATE_Z2_Z1_0 = 0.2,
    interaction = 0
  )  %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

