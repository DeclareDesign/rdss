## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------
# Discovery using causal forests
# ------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
covariates <- paste0("X.", 1:10)

f_Y <- function(z, X.1, X.2, X.3, X.4, u) {
  z * X.1 + z * X.2 ^ 2 + z * exp(X.3) + z * X.3 * X.4 + u
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_predictor <- function(data) {
  data.frame(
    inquiry = "best_predictor",
    estimand = lapply(covariates, function(j) {
      lm_robust(tau ~ cut(data[[j]], 20), data = data)$r.squared
    }) %>%
      unlist %>% which.max
  )
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
causal_forest_handler <- function(data, ...) {

  X <- as.matrix(data %>% select(all_of(covariates)))
  train <- data$train
  
  cf <- causal_forest(X = X[train, ], Y = data$Y[train], W = data$Z[train], ...) 
  
  # Prep and return data
  data$pred <- NA
  data$pred[train]  <- predict(cf, estimate.variance=FALSE)$predictions
  data$pred[!train] <- predict(cf, newdata=X[!train,], estimate.variance=FALSE)$predictions

data %>%
  mutate(var_imp = variable_importance(cf) %>% which.max,
         low_test  = (!train & (pred < quantile(pred[!train], .2))),
         high_test = (!train & (pred > quantile(pred[!train], .8))),
         low_all = pred < quantile(pred, .2))
}

take_1 <-
  function(data) {
    data %>%
      slice(1) %>%
      mutate(estimate = var_imp) %>%
      select(estimate)
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
N <- 1000

design <- 
  declare_model(
    N = N,  
    X = matrix(rnorm(10*N), N),
    u = rnorm(N),
    Z = sample(0:1, N, replace = TRUE)) + 
  declare_measurement(handler = fabricate,
    Y_1 = f_Y(1, X.1, X.2, X.3, X.4, u),
    Y_0 = f_Y(0, X.1, X.2, X.3, X.4, u), 
    tau = Y_1 - Y_0,
    Y = f_Y(Z, X.1, X.2, X.3, X.4, u), 
    train = simple_rs(N)==1) +
  declare_inquiry(handler = best_predictor, label = "custom") +
  declare_step(handler = causal_forest_handler) +
  declare_inquiry(
    worst_effects = mean(tau[tau <= quantile(tau, .2)]),
    weak_effects = mean(tau[low_test]),
    weak_all = mean(tau[low_all]),
    strong_effects = mean(tau[high_test])) +
  declare_estimator(Y ~ Z, model = lm_robust, subset = low_test, 
                    inquiry = c("weak_effects", "worst_effects"), label = "lm_weak") +
  declare_estimator(Y ~ Z, model = lm_robust, subset = low_all, 
                    inquiry = "weak_all", label = "lm_weak_all") +
  declare_estimator(Y ~ Z, model = lm_robust, subset = high_test, 
                    inquiry = "strong_effects", label = "lm_strong") +
  declare_estimator(handler = label_estimator(take_1),
                    inquiry = "best_predictor", label = "cf") 


