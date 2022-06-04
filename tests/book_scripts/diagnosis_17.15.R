library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_17.16.R")
diagnosis_17.15 <- diagnose_design(declaration_17.16, sims = sims, bootstrap_sims = bootstrap_sims)

