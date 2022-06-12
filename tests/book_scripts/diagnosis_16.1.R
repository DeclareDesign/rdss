print('diagnosis_16.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_16.2.R")
diagnosis_16.1 <- 
  declaration_16.2 |> 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

