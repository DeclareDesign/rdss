print('diagnosis_16.3.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_16.4.R")
diagnosis_16.3 <- 
  declaration_16.4 %>% 
  redesign(proportion_hiding = seq(from = 0, to = 0.3, by = 0.1), 
           N = seq(from = 500, to = 2500, by = 500)) %>% 
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims)

