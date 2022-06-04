library(DeclareDesign); library(rdddr); library(tidyverse)


sims <- 3
p.values <- rep(NA, sims)
for(i in 1:sims){
  Z <- rbinom(100, 1, 0.5)
  U <- rnorm(100)
  Y <- 0.2 * Z + U
  p.values[i] <- summary(lm(Y ~ Z))$coefficients[2, 4]
}
power <- mean(p.values <= 0.05)
power
