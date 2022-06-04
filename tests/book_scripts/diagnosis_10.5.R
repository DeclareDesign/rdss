library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- FALSE
source("../book_scripts/declaration_10.3.R")
diagnosis_10.5 <-
  diagnose_design(
    list(declaration_10.3a, declaration_10.3b),
    sims = sims,
    bootstrap_sims = bootstrap_sims
  )

