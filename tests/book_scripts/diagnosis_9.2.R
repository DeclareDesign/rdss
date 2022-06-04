library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
set.seed(42)
sims <- 3
b_sims <- 3
source("../book_scripts/declaration_9.1.R")
source("../book_scripts/declaration_9.2.R")
diagnosis_9.2 <- diagnose_design(declaration_9.2, sims = sims, bootstrap_sims = b_sims)

