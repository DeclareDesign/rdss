print('a_simulation_10.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
set.seed(343)
source("../book_scripts/declaration_10.1.R")
simulation_10.1 <- 
  simulate_design(declaration_10.1, sims = 10) |>
  mutate(significant = p.value <= 0.05)

