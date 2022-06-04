library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_21.x2.R")
diagnosis_21.2 <- 
  declaration_21.x2 %>%
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims)

