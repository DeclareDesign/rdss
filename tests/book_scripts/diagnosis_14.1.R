library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_14.1.R")
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2))
)
diagnosis_14.1 <- diagnose_design(declaration_14.1, diagnosands = diagnosands, sims = sims, bootstrap_sims = bootstrap_sims) 

