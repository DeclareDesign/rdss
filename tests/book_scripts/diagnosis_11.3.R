library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- FALSE
source("../book_scripts/declaration_11.3.R")
diagnosis_11.3 <-
  diagnose_designs(declaration_11.3, sims = sims, bootstrap_sims = bootstrap_sims)

