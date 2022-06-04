library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_15.2.R")
diagnosis_15.2 <- diagnose_design(declaration_15.2,
                                  sims = sims, bootstrap_sims = bootstrap_sims)

