library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_17.6.R")
diagnosis_17.6 <-
  declaration_17.6 %>%
  redesign(ICC = seq(0.1, 0.9, by = 0.4)) %>% 
  diagnose_designs()

