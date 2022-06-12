print('declaration_18.3a.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


# Equilibrium offers for a game of length n
offer <- function(n, d){
  sum(sapply(2:n[1], function(t) ((-1)^t)*(d^{t-1})))
}
# Likelihood function
likelihood  <- function(n){
  function(k, d, a) {
    m <- Z * offer(n, d) + (1 - Z) * (1 - offer(n, d))
    R <- a * dbeta(y, k * .75, k * .25) + 
      (1 - a) * dbeta(y, k * m, k * (1 - m))
    return(-sum(log(R)))
  }
}
