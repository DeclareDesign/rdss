library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- FALSE
source("../book_scripts/declaration_14.3_LOGIT.R")
diagnosis_11.3_LOGIT <-
  declaration_14.3_LOGIT %>%
  redesign(N = seq(10, 100, by = 10)) %>%
  diagnose_designs(sims = sims, 
                   bootstrap_sims = bootstrap_sims)

