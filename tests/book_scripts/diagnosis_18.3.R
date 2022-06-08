print('diagnosis_18.3.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_18.3a.R")
source("../book_scripts/declaration_18.3b.R")
diagnosis_18.3a <- diagnose_design(declaration_18.3) 
diagnosis_18.3b <- 
  declaration_18.3 %>%
  redesign(alpha = c(.25, .5, .75), n = c(2, 8)) %>% 
  diagnose_design(sims = sims, bootstrap_sims = bootstrap_sims)
# Note that when data is too noisy there is a possibility of convergence failure
# diagnosis_18.3b$simulations_df %>%
#  group_by(horizon, fixed) %>%
#  summarize('Share failing' = mean(is.na(std.error)))




