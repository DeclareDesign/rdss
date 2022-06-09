print('diagnosis_16.6.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_16.6_a.R")
source("../book_scripts/declaration_16.6_b.R")
diagnosis_16.6 <-
  declaration_16.6 %>%
  redesign(deceive = c(TRUE, FALSE)) %>%
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims) 


