library(DeclareDesign); library(rdddr); library(tidyverse)


structural_estimator <- 
  function(data, pi, y, chi = 3/4){
    # Define negative log likelihood as a function of k, d and q
    LL  <- function(k, d, q) {
      m <- with(data, y(Z, d))
      R <- q * dbeta(data[pi][[1]], k * chi, k * (1- chi)) +
        (1 - q) * dbeta(data[pi][[1]], k * m, k * (1 - m))
      - sum(log(R))
    }
    # Estimation
    M <- mle2(
      LL,
      method = "L-BFGS-B",
      start = list(k = 2, d = 0.50,  q = 0.50),
      lower = c(k = 1,    d = 0.01,  q = 0.01),
      upper = c(k = 1000, d = 0.99,  q = 0.99)
    )
    # Format output from estimation
    out <- data.frame(coef(summary(M)), outcome = pi)
    names(out) <- c("estimate", "std.error", "statistic", "p.value", "outcome")
    # Use estimates of q and delta to predict average treatment effects (ATEs)
    # Predicted ATE for n=2
    out[4, 1] <- (1 - out["q", "estimate"]) * (2 * out["d", "estimate"] - 1)
    # Predicted ATE for n=infinity
    out[5, 1] <- (1 - out["q", "estimate"]) * (2 * out["d", "estimate"] /
                                                 (1 + out["d", "estimate"]) - 1)
    out
  }
