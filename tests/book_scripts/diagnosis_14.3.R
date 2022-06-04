library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_14.3a.R")
source("../book_scripts/declaration_14.3b.R")
source("../book_scripts/declaration_14.3c.R")
diagnosis_14.3 <- 
  declaration_14.3 %>% 
  redesign(cluster_prob = seq(0.1, 0.9, 0.1)) %>% 
  diagnose_designs(sims = sims, bootstrap_sims = bootstrap_sims)

