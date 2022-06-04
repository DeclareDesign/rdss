library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_17.15.R")
diagnosis_17.14 <- diagnose_design(declaration_17.15, sims = sims, bootstrap_sims = bootstrap_sims)

