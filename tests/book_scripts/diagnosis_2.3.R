library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
set.seed(42)
sims <- 20
b_sims <- FALSE
source("../book_scripts/declaration_2.1.R")
source("../book_scripts/declaration_2.2.R")
diagnosands <-
  declare_diagnosands(
    correct_call_rate = mean((estimate > 0.5) == (estimand > 0.5))
  )
diagnosis_2.3 <- 
  diagnose_design(
    design_1 = steady_race + single_poll,
    design_2 = steady_race + three_polls,
    design_3 = october_surprise + single_poll,
    design_4 = october_surprise + three_polls,
    diagnosands = diagnosands,
    sims = sims, bootstrap_sims = b_sims
  )

