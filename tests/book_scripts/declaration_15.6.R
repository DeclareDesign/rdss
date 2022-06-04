library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_15.6 <-
  declaration_15.5 + 
  declare_estimator(
    Y ~ X * D, 
    subset = X > -1*bandwidth & X < bandwidth,
    model = lm_robust, 
    inquiry = "LATE",
    label = "linear"
  )
