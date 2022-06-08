print('diagnosis_14.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_14.1.R")
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2))
)
diagnosis_14.1 <- diagnose_design(declaration_14.1, diagnosands = diagnosands, sims = sims, bootstrap_sims = bootstrap_sims) 

