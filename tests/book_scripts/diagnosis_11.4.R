library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- FALSE
source("../book_scripts/declaration_11.4.R")
diagnosis_11.4 <-
  declaration_11.4 %>%
  redesign(N = c(100, 500, 1000)) %>%
  diagnose_designs()

