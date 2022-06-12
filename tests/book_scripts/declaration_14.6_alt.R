print('declaration_14.6_alt.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


declaration_14.6 <-
  declare_model(
    N = 500,
    Z = complete_ra(N),
    potential_outcomes(Y1 ~ .1 + .3*Z + rnorm(N)),
    potential_outcomes(Y2 ~ .3 + .5*Z + rnorm(N)/2),
    potential_outcomes(Y3 ~ 0  + 0*Z + rnorm(N))) +
  declare_measurement(Y1 = reveal_outcomes(Y1 ~ Z),
                      Y2 = reveal_outcomes(Y2 ~ Z),
                      Y3 = reveal_outcomes(Y3 ~ Z)) +
  declare_inquiry(average_Y_1 = 
                    (mean((Y1_Z_1 - mean(Y1_Z_0))/sd(Y1_Z_0)) + 
                     mean((Y2_Z_1 - mean(Y2_Z_0))/sd(Y2_Z_0)) +
                     mean((Y3_Z_1 - mean(Y3_Z_0))/sd(Y3_Z_0)))/3) +
  declare_measurement(
    scale_1 =  
      ((Y1 - mean(Y1[Z==0]))/sd(Y1[Z==0]) + 
       (Y2 - mean(Y2[Z==0]))/sd(Y2[Z==0]) +
       (Y3 - mean(Y3[Z==0]))/sd(Y3[Z==0]))/3) +
  declare_measurement(scale_2 = scale(scale_1)) +
  declare_estimator(
    scale_1 ~  1,
    model = lm_robust,
    subset = Z == 1,
    label = "simple"
  ) +
  declare_estimator(
    scale_2 ~  1,
    model = lm_robust,
    subset = Z == 1,
    label = "rescaled"
  )
draw_estimates(declaration_14.6) %>% head
draw_data(declaration_14.6) %>% head
diagnose_design(declaration_14.6, sims = 100)
