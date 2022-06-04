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


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ------------------------
# Selection-on-observables
# ------------------------

## ----file = "scripts_declarations/declaration_15.2.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_15.2 <- diagnose_design(declaration_15.2)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -----------------------------------------
# Selection-on-observables :: Exercises {-}
# -----------------------------------------

## -------------------------------------------------------------------------------------------------------
library(sensemakr)
sensitivity_helper <-
  function(model) {
    sensitivity <- sensemakr(model = model, treatment = "Z")
    sensitivity$sensitivity_stats[,c("estimate", "rv_qa")] 
  }


## -------------------------------------------------------------------------------------------------------
design <-
  declare_model(N = 1000, 
                     U = rnorm(N, sd = 0.6),
                     X = correlate(rnorm, given = U, rho = 0.5),
                     Z = rbinom(N, 1, prob = pnorm(X)),
                     Y = 0.2 * Z + 0.5 * X + U) +
  declare_estimator(Y ~ Z + X, model = lm, model_summary = sensitivity_helper)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------
# Difference-in-differences
# -------------------------

## ----file = "scripts_declarations/declaration_15.3.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_15.3 <- diagnose_design(declaration_15.3)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------------
# Instrumental variables
# ----------------------

## ----file = "scripts_declarations/declaration_15.4.R"---------------------------------------------------


## ---- echo = TRUE---------------------------------------------------------------------------------------
# --------------------------------
# Regression discontinuity designs
# --------------------------------

## ----file = "scripts_declarations/declaration_15.5.R"---------------------------------------------------


## ----file = "scripts_declarations/declaration_15.6.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_15.5 <- 
  declaration_15.6 %>% 
  redesign(bandwidth = seq(from = 0.05, to = 0.5, by = 0.05)) %>% 
  diagnose_designs


