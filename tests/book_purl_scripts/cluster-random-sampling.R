library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# -----------------------
# Cluster random sampling
# -----------------------

## ----file = "scripts_declarations/declaration_14.3a.R"--------------------------------------------------


## ----file = "scripts_declarations/declaration_14.3b.R"--------------------------------------------------


## ----file = "scripts_declarations/declaration_14.3c.R"--------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
designs <- redesign(declaration_14.3, 
                    cluster_prob = seq(0.1, 0.9, 0.1))
diagnosis_14.3 <- diagnose_design(designs)


