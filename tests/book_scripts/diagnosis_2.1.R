library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
set.seed(42)
sims <- 3
b_sims <- FALSE
source("../book_scripts/declaration_2.1.R")
diagnosands <-
  declare_diagnosands(
    correct_call_rate = mean((estimate > 0.5) == (estimand > 0.5))
  )
diagnosis_2.1 <- diagnose_design(design = declaration_2.1, diagnosands = diagnosands, sims = sims, bootstrap_sims = b_sims)

