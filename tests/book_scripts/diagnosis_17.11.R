print('diagnosis_17.11.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_17.11a.R")
source("../book_scripts/declaration_17.11b.R")
source("../book_scripts/declaration_17.11c.R")
diagnosis_17.11a <- 
  encouragement_design |> 
  redesign(compliance_rate = seq(0.1, 0.9, by = 0.1)) |> 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)
diagnosis_17.11b <- 
  placebo_controlled_design |> 
  redesign(compliance_rate = seq(0.1, 0.9, by = 0.1)) |> 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)


