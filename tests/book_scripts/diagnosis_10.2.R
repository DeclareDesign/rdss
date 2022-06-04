library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
# to match the user-facing code
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_10.1.R")
diagnosands <- declare_diagnosands(power = mean(p.value <= 0.05))
diagnosis_10.2 <- 
  diagnose_design(declaration_10.1, 
                  diagnosands = diagnosands,
                  sims = sims, bootstrap_sims = bootstrap_sims)

