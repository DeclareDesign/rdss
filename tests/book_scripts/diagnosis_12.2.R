library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_12.1a.R")
source("../book_scripts/declaration_12.1b.R")
source("../book_scripts/declaration_12.1c.R")
source("../book_scripts/declaration_12.1d.R")
declaration_12.1 <-
  model_12.1 + inquiry_12.1 + data_strategy_12.1 + answer_strategy_12.1
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2)),
  power = mean(p.value <= 0.05),
  cost = mean(10 * n_villages + 1 * n_villages * citizens_per_village)
)
diagnosis_12.2 <-
  declaration_12.1 %>%
  redesign(n_villages = c(192, 500),
           citizens_per_village = c(25, 50, 75, 100)) %>%
  diagnose_designs(diagnosands = diagnosands,
                   sims = sims,
                   bootstrap_sims = bootstrap_sims)

