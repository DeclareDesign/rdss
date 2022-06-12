print('diagnosis_21.2.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_21.x2.R")
diagnosis_21.2 <- 
  declaration_21.x2 |>
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims)

