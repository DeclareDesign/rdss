print('diagnosis_19.4_NEW.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
sims <- 3
bootstrap_sims <- 100
source("../book_scripts/declaration_19.4_NEW.R")
diagnosis_19.4_NEW.R <- diagnose_designs(design_coordinated = design_coordinated, 
                                design_uncoordinated = design_uncoordinated,
                                sims = sims,
                                bootstrap_sims =bootstrap_sims)

#          file = "~/Dropbox/DeclareDesign_book_rfiles/dataverse/diagnosis_19.4_NEW.R.rds")

          file = "c:/Dropbox (WZB)/DeclareDesign_book_rfiles/dataverse/diagnosis_19.4_NEW.R.rds")
