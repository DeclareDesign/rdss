library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- FALSE
source("../book_scripts/declaration_11.2.R")
diagnosands <-
  declare_diagnosands(cost = unique(N * 2 + prob * N * 20),
                      rmse = sqrt(mean((estimate - estimand) ^ 2)))
diagnosis_11.2 <-
  declaration_11.2 %>%
  redesign(N = seq(100, 1000, 50),
           prob = seq(0.1, 0.5, 0.2)) %>%
  diagnose_designs(diagnosands = diagnosands, sims = sims, bootstrap_sims = bootstrap_sims)

