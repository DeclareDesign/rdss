print('diagnosis_15.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(DeclareDesign)
library(tidyverse)
library(rdddr)
library(CausalQueries)
sims <- 3
bootstrap_sims <- 3
source("../book_scripts/declaration_15.1.R")
diagnosis_15.1 <- diagnose_design(declaration_15.1,
                                  sims = sims, 
                                  bootstrap_sims = bootstrap_sims,
                                  make_groups = vars(XY))
# diagnosis_15.1$diagnosands_df |> ggplot(aes(term, rmse)) + geom_point() +  facet_wrap(~XY) + theme_bw() + 
# xlab("Strategy") + geom_errorbar(aes(ymin =  rmse - 1.96*`se(rmse)`, ymax = rmse + 1.96*`se(rmse)`, width = .2))


