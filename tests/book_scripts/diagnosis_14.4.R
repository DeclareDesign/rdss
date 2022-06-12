print('diagnosis_14.4.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_14.4.R")
source("../book_scripts/declaration_14.5.R")
diagnosis_14.4 <- 
  declaration_14.5 |> 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

