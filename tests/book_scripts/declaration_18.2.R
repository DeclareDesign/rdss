library(DeclareDesign); library(rdddr); library(tidyverse)


f_Y <- function(z, X.1, X.2, X.3, X.4, u) {
  z * X.1 + z * X.2 ^ 2 + z * exp(X.3) + z * X.3 * X.4 + u
}
get_best_predictor <-
  function(data) {
    data %>%
      slice(1) %>%
      transmute(estimate = var_imp) 
  }
declaration_18.2 <- 
  declare_model(
    N = 1000,  
    X = matrix(rnorm(10 * N), N),
    U = rnorm(N),
    Z = simple_ra(N)) + 
  declare_measurement(handler = fabricate,
                      Y_Z_1 = f_Y(1, X.1, X.2, X.3, X.4, U),
                      Y_Z_0 = f_Y(0, X.1, X.2, X.3, X.4, U), 
                      tau = Y_Z_1 - Y_Z_0,
                      Y = reveal_outcomes(Y ~ Z), 
                      train = simple_rs(N) == 1) +
  declare_inquiry(handler = best_predictor, covariates = paste0("X.", 1:10), label = "custom") +
  declare_step(handler = causal_forest_handler, covariates = paste0("X.", 1:10)) +
  declare_inquiry(
    worst_effects = mean(tau[tau <= quantile(tau, 0.2)]),
    weak_effects = mean(tau[low_test]),
    weak_all = mean(tau[low_all]),
    strong_effects = mean(tau[high_test])) +
  declare_estimator(Y ~ Z, model = lm_robust, subset = low_test, 
                    inquiry = c("weak_effects", "worst_effects"), label = "lm_weak") +
  declare_estimator(Y ~ Z, model = lm_robust, subset = low_all, 
                    inquiry = "weak_all", label = "lm_weak_all") +
  declare_estimator(Y ~ Z, model = lm_robust, subset = high_test, 
                    inquiry = "strong_effects", label = "lm_strong") +
  declare_estimator(handler = label_estimator(get_best_predictor),
                    inquiry = "best_predictor", label = "cf") 
