print('diagnosis_17.6.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_17.6.R")
diagnosis_17.6 <-
  declaration_17.6 %>%
  redesign(ICC = seq(0.1, 0.9, by = 0.4)) %>% 
  diagnose_designs()

