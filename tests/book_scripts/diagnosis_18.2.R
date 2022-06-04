library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_18.2.R")
most_common <- 
  function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
discovery_diagnosands  <- 
  declare_diagnosands(
    correct = mean(estimate == estimand),
    bias = mean(estimate - estimand),
    mean_estimate = mean(estimate),
    modal_estimate = most_common(round(estimate, 1)),
    mean_estimand = mean(estimand),
    modal_estimand = most_common(round(estimand, 1))
  )
diagnosis_18.2 <- diagnose_design(declaration_18.2, diagnosands = discovery_diagnosands, sims = sims, bootstrap_sims = bootstrap_sims) 

