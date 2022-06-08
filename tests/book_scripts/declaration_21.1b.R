print('declaration_21.1b.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


slope <- function(y, x) { cov(y, x) / var(x) }
inquiry <-  
  declare_inquiries(
    # Average effects
    ATE_nationalism = 
      mean(blm_support_Z_nationalism - blm_support_Z_general),
    ATE_feminism = 
      mean(blm_support_Z_feminism - blm_support_Z_general),
    ATE_intersectional = 
      mean(blm_support_Z_intersectional - blm_support_Z_general),
    # Overall heterogeneity w.r.t. blm_familiarity
    DID_nationalism_familiarity = 
      slope(blm_support_Z_nationalism - blm_support_Z_general, 
            blm_familiarity),
    DID_feminism_familiarity = 
      slope(blm_support_Z_feminism - blm_support_Z_general, 
            blm_familiarity),
    DID_intersectional_familiarity = 
      slope(blm_support_Z_intersectional - blm_support_Z_general, 
            blm_familiarity),
    # Treatment-specific heterogeneity
    DID_nationalism_linked_fate = 
      slope(blm_support_Z_nationalism - blm_support_Z_general, 
            linked_fate),
    DID_feminism_gender = 
      slope(blm_support_Z_feminism - blm_support_Z_general,
            female),
    DID_intersectional_lgbtq = 
      slope(blm_support_Z_intersectional - blm_support_Z_general, 
            lgbtq)
  )
