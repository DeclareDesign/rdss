library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_9.2 <- 
  declaration_9.1 +
  declare_test(age ~ 1, 
               linear_hypothesis = "(Intercept) = 20", 
               model = lh_robust, label = "test")
