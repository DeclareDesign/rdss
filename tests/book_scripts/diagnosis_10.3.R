print('diagnosis_10.3.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_10.1.R")
diagnosis_10.3 <-
  diagnose_design(
    declaration_10.1,
    diagnosands = declare_diagnosands(
      bias = mean(estimate - estimand),
      true_se = sd(estimate),
      power = mean(p.value <= 0.05),
      coverage = mean(estimand <= conf.high &
                        estimand >= conf.low)
    ),
    sims = sims,
    bootstrap_sims = bootstrap_sims
  )

