library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_18.1_NEW.R")
diagnosis_18.1 <- diagnose_design(declaration_18.1, sims = sims, bootstrap_sims = bootstrap_sims) 

