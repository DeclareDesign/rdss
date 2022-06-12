print('diagnosis_17.3_CONJOINT.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 500
bootstrap_sims <- 500
source("../book_scripts/declaration_17.3_CONJOINT.R")
diagnosis_17.3_CONJOINT <- 
  declaration_17.3_CONJOINT |> 
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims)

