library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_17.5.R")
diagnosis_17.5 <- 
  declaration_17.5 %>% 
  redesign(
    control_slope = seq(-1, 1, 0.5), 
    prob = seq(0.1, 0.9, 0.1)) %>% 
  diagnose_designs()

