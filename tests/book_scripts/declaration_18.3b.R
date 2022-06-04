library(DeclareDesign); library(rdddr); library(tidyverse)


library(bbmle)
d = 0.8       # True delta (unknown)
k = 6         # Parameter to governance variance (unknown)
q = 0.5       # Share of behavioral types in the population (unknown)
chi = 0.75    # Price paid by norm following ("behavioral" customers) (known)
declaration_18.3 <- 
  declare_model(
    # Define the population: indicator for behavioral type (norm = 1)
    N = 500, norm = rbinom(N, 1, q),
    # Define mean potential outcomes for n = 2 
    potential_outcomes(
      pi_two ~ norm*chi + (1-norm)*(Z*d + (1-Z)*(1-d))
    ),
    # Define mean potential outcomes for n = infinity
    potential_outcomes(
      pi_inf ~ norm*chi + (1-norm)*(Z*d/(1+d) + (1-Z)*(1-d/(1+d)))
    )
  ) +
  declare_inquiry(ATE_two = mean(pi_two_Z_1 - pi_two_Z_0), # ATE n = 2
                  ATE_inf = mean(pi_inf_Z_1 - pi_inf_Z_0), # ATE n = infinity
                  k = k,                                   # kappa
                  d = d,                                   # delta
                  q = q) +                                 # q
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(
    pi_two = reveal_outcomes(pi_two ~ Z),
    pi_inf = reveal_outcomes(pi_inf ~ Z),
    # Get draws from beta distribution given means for n = 2 and n = infinity
    pi_two_obs = rbeta(N, pi_two*k, (1-pi_two)*k),      
    pi_inf_obs = rbeta(N, pi_inf*k, (1-pi_inf)*k)
  ) +
  # Difference-in-means for n = 2
  declare_estimator(pi_two_obs ~ Z, inquiry = "ATE_two", label = "DIM_two") +
  # Difference-in-means for n = infinity
  declare_estimator(pi_inf_obs ~ Z, inquiry = "ATE_inf", label = "DIM_inf") +
  # MLE for n = 2
  declare_estimator(handler = label_estimator(structural_estimator), 
                    pi = "pi_two_obs", 
                    y = function(Z, d) Z * d + (1 - Z) * (1 - d), 
                    inquiry = c("k","d", "q", "ATE_two", "ATE_inf"), 
                    label = "Struc_two") +
  # MLE for n = infinity
  declare_estimator(handler = label_estimator(structural_estimator),
                    pi = "pi_inf_obs", 
                    y = function(Z, d) Z*d/(1+d) +  (1-Z)*(1-d/(1+d)),
                    inquiry = c("k","d","q","ATE_two", "ATE_inf"), 
                    label = "Struc_inf") 
