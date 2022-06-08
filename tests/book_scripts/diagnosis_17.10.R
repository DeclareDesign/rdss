print('diagnosis_17.10.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_17.9.R")
diagnosis_17.10 <- diagnose_design(declaration_17.9, sims = sims, bootstrap_sims = bootstrap_sims)

