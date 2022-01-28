#' Tidy causal forest results
#'
#' @param data A data.frame
#' @param ... Options to causal_forest
#'
#' @export
#'
#' @importFrom grf causal_forest variable_importance
#' @importFrom dplyr mutate select all_of `%>%`
#' @importFrom stats quantile
causal_forest_handler <- function(data, ...) {

  X <- as.matrix(data %>% select(all_of(covariates)))
  train <- data$train

  cf <-
    causal_forest(X = X[train,],
                  Y = data$Y[train],
                  W = data$Z[train],
                  ...)

  # Prep and return data
  data$pred <- NA
  data$pred[train]  <-
    predict(cf, estimate.variance = FALSE)$predictions
  data$pred[!train] <-
    predict(cf, newdata = X[!train, ], estimate.variance = FALSE)$predictions

  data %>%
    mutate(
      var_imp = variable_importance(cf) %>% which.max,
      low_test  = (!train &
                     (pred < quantile(pred[!train], .2))),
      high_test = (!train &
                     (pred > quantile(pred[!train], .8))),
      low_all = pred < quantile(pred, .2)
    )
}


#' Best predictor function from causal_forest
#'
#' @param data A data.frame
#'
#' @export
#'
#' @importFrom estimatr lm_robust
best_predictor <- function(data) {
  data.frame(
    inquiry = "best_predictor",
    estimand = lapply(covariates, function(j) {
      lm_robust(tau ~ cut(data[[j]], 20), data = data)$r.squared
    }) %>%
      unlist %>% which.max
  )
}
