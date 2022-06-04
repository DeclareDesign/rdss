library(DeclareDesign); library(rdddr); library(tidyverse)


data_strategy <- 
  declare_assignment(
    Z = simple_ra(
      N,
      conditions = 
        c("general", "nationalism", "feminism", "intersectional"), 
      simple = TRUE
    )
  ) + 
  declare_measurement(blm_support = reveal_outcomes(blm_support ~ Z)) 
