print('declaration_18.3b.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


library(bbmle)
n = 2        # Number of rounds bargaining (design choice)
delta = 0.8  # True delta (unknown)
kappa = 2    # Parameter to govern error in offers (unknown)
alpha = 0.5  # Share of behavioral types in the population (unknown)
declaration_18.3 <- 
  declare_model(
    # Define the population: indicator for behavioral type (norm = 1)
    N = 200, 
    type = rbinom(N, 1, alpha),
    n = n) +
  declare_inquiry(kappa = kappa,     
                  delta = delta,     
                  alpha = alpha) +   
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(
    # Equilibrium payoff
    pi = type*.75 + (1-type)*(Z*offer(n, delta) + (1-Z)*(1-offer(n, delta))),
    # Actual payoff (stochastic)
    y = rbeta(N, pi*kappa, (1-pi)*kappa)) +
  # Estimation via maximum likelihood
  declare_estimator(.method = bbmle::mle2,
                    minuslogl =  likelihood(n),
                    start = list(k = 2,    d = 0.50,  a = 0.50),
                    lower = list(k = .1,   d = 0.01,  a = 0.01),
                    upper = list(k = 100,  d = 0.99,  a = 0.99),
                    method = "L-BFGS-B",
                    term = c("k", "d", "a"),
                    inquiry = c("kappa","delta", "alpha"), 
                    label = "Structural model") 
