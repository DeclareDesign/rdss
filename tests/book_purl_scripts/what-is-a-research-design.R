library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------------------------------------------------
# Example: October surprise :: A "steady race" design
# ---------------------------------------------------

## ----file = "scripts_declarations/declaration_2.1.R"----------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosands <-
  declare_diagnosands(
    correct_call_rate = mean((estimate > 0.5) == (estimand > 0.5))
  )

diagnosis_2.1 <-
  diagnose_design(design = declaration_2.1,
                  diagnosands = diagnosands)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------------------------------------------------------
# Example: October surprise :: An "October surprise" design
# ---------------------------------------------------------

## ----file = "scripts_declarations/declaration_2.2.R"----------------------------------------------------



## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_2.2 <-
  diagnose_design(design = declaration_2.2,
                  diagnosands = diagnosands)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------------------------------------------------------------
# Example: October surprise :: Choosing between empirical designs
# ---------------------------------------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_2.3 <- 
  diagnose_design(
     design_1 = steady_race + single_poll,
     design_2 = steady_race + three_polls,
     design_3 = october_surprise + single_poll,
     design_4 = october_surprise + three_polls,
    diagnosands = diagnosands
  )


