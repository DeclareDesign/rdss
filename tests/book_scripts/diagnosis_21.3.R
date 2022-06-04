library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_21.1a.R")
source("../book_scripts/declaration_21.1b.R")
source("../book_scripts/declaration_21.1c.R")
source("../book_scripts/declaration_21.1d.R")
source("../book_scripts/declaration_21.1e.R")
diagnosis_21.3 <- 
  declaration_21.1 %>%
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims)

