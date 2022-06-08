print('declaration_12.1d.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


answer_strategy_12.1 <- 
  declare_estimator(Y_observed ~ Z, term = c("Zpersonal", "Zsocial"), 
                    clusters = villages, 
                    method = lm_robust,
                    se_type = "stata",
                    inquiry = c("ATE_personal", "ATE_social"))
