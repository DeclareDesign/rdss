library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_10.4.R")
diagnosis_10.6 <-
  diagnose_design(
    declaration_10.4,
    sims = sims,
    bootstrap_sims = bootstrap_sims
  )

