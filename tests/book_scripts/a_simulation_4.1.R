print('a_simulation_4.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
source("../book_scripts/declaration_4.1.R")
simulation_4.1 <- simulate_design(declaration_4.1, sims = sims)

