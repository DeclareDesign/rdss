library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_16.6.R")
diagnosis_16.5 <- diagnose_design(declaration_16.6, sims = sims, bootstrap_sims = bootstrap_sims) 

