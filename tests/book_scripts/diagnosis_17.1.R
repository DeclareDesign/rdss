print('diagnosis_17.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/designer_17.1.R")
diagnosands <- 
  declare_diagnosands(
    power = mean(p.value <= 0.05),
    bias = mean(estimate - estimand),
    coverage = mean(estimand <= conf.high & estimand >= conf.low),
    variance = pop.var(estimate)
  )
diagnosis_17.1 <- 
  expand_design(designer = designer_17.1,
                N = 100,
                m = seq(10, 90, 10),
                var_Y0 = 1,
                var_Y1 = 2,
                cov_Y0_Y1 = 0.5,
                mean_Y0 = 1.0,
                mean_Y1 = 1.75) %>% 
  diagnose_designs(diagnosands = diagnosands, sims = sims, bootstrap_sims = bootstrap_sims)

