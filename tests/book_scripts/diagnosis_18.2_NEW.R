print('diagnosis_18.2_NEW.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_18.2_NEW.R")
diagnosis_18.2 <- 
  declaration_18.2 |> 
  redesign(N = seq(100, 1100, by = 200),
           r_sq = seq(0.0, 0.8, by = 0.2)) |> 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims) 

