library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_16.6.R")
diagnosis_16.6 <-
  declaration_16.6 %>%
  replace_step(3, declare_assignment(pair = complete_ra(N = N, num_arms = n_pairs),
                                     role = 1 + block_ra(blocks = pair))) %>% 
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims) 

