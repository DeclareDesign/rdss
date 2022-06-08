print('diagnosis_9.4.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
set.seed(42)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_9.4.R")
diagnosis_9.4 <- diagnose_design(declaration_9.4, sims = sims, bootstrap_sims = bootstrap_sims)

