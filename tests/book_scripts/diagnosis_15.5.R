library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_15.5.R")
source("../book_scripts/declaration_15.6.R")
diagnosis_15.5 <- 
  declaration_15.6 %>% 
  redesign(bandwidth = seq(from = 0.05, to = 0.5, by = 0.05)) %>% 
  diagnose_designs

