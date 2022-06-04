library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_15.3.R")
diagnosis_15.3 <- diagnose_design(declaration_15.3,
                                  sims = sims, bootstrap_sims = bootstrap_sims)

