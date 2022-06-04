library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_18.1_NEW.R")
diagnosis_18.1 <- diagnose_design(declaration_18.1, sims = sims, bootstrap_sims = bootstrap_sims) 

