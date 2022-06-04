library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
set.seed(42)
sims <- 3
b_sims <- FALSE
source("../book_scripts/declaration_9.1.R")
diagnosis_9.1 <- diagnose_design(declaration_9.1, sims = sims, bootstrap_sims = b_sims)

