library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------------
# Process tracing
# ---------------

## -------------------------------------------------------------------------------------------------------
library(CausalQueries)

causal_model <- make_model("X -> M -> Y <- W -> M") %>%
  
  set_restrictions("(M[X=1] < M[X=0]) | (M[X=1, W=1] == M[X=0, W=1])") %>%
  
  set_restrictions("(Y[M=1] < Y[M=0]) | (Y[M=1, W=1] == Y[M=0, W=1])")


## ----file = "scripts_declarations/declaration_15.1.R", eval = TRUE--------------------------------------


