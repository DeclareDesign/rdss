print('diagnosis_18.4.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 1000
bootstrap_sims <- 500
source("../book_scripts/declaration_18.4.R")
diagnosis_18.4 <- diagnose_design(declaration_18.4, sims = sims, bootstrap_sims = bootstrap_sims) 

