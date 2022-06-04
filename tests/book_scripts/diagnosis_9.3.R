library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
set.seed(42)
sims <- 20
bootstrap_sims <- 20
observed_estimate <-
  difference_in_means(views ~ success, data = clingingsmith_etal)
source("../book_scripts/declaration_9.3.R")
p.value <-
  declare_diagnosands(
    p.value = mean(abs(estimate) >= abs(observed_estimate$coefficients))
  )
diagnosis_9.3 <-
  diagnose_design(
    null_design,
    diagnosands = p.value,
    sims = sims,
    bootstrap_sims = bootstrap_sims
  )

