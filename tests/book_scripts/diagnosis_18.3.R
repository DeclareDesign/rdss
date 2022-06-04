library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_18.3a.R")
source("../book_scripts/declaration_18.3b.R")
diagnosis_18.3 <- diagnose_design(declaration_18.3, sims = sims, bootstrap_sims = bootstrap_sims) 

