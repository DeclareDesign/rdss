library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 20
bootstrap_sims <- 20
source("../book_scripts/declaration_16.3.R")
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  mean_CI_width = mean(conf.high - conf.low)
)
diagnosis_16.2 <- diagnose_design(declaration_16.3, sims = sims, bootstrap_sims = bootstrap_sims, diagnosands = diagnosands)

