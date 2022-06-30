#' Tidy helper function for causal_forest function
#'
#' Runs estimates estimation function from interference package and returns tidy data frame output
#'
#' https://draft.declaredesign.org/complex-designs.html#discovery-using-causal-forests
#'
#' See ?causal_forest for further details
#'
#' @param data A data.frame
#' @param covariate_names Names of covariates
#' @param share_train Share of units to be used for training
#' @param ... Options to causal_forest
#'
#' @return a data.frame of estimates
#'
#' @export
#'
#' @importFrom dplyr mutate select all_of `%>%`
#' @importFrom stats quantile predict
#' @importFrom randomizr complete_rs
#'
#' @examples
#'
#' library(DeclareDesign)
#' library(ggplot2)
#'
#' dat <- fabricate(
#'    N = 1000,
#'    A = rnorm(N),
#'    B = rnorm(N),
#'    Z = complete_rs(N),
#'    Y = A*Z + rnorm(N))
#'
#' # note: remove num.threads = 1 to use more processors
#' estimates <- causal_forest_handler(data = dat, covariate_names = c("A", "B"), num.threads = 1)
#'
#' ggplot(data = estimates, aes(A, pred)) + geom_point()
#'
causal_forest_handler <- function(data, covariate_names, share_train = 0.5, ...) {

  if(!requireNamespace("grf")){
    message("The causal_forest_helper function requires the 'grf' package.")
    return(invisible())
  }

  X <- as.matrix(data %>% select(all_of(covariate_names)))

  train <- complete_rs(nrow(data), prob = share_train) == 1

  cf <-
    grf::causal_forest(X = X[train,],
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
    mutate(var_imp = grf::variable_importance(cf) %>% which.max,
           train = train,
           test = !train)
}


#' Best predictor function from causal_forest
#'
#' @param data A data.frame of covariates
#' @param covariate_names A character vector of covariates to assess
#' @param cuts Either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which each covariate is to be cut.
#'
#' @return a data.frame of the best predictors
#'
#' @export
#'
#' @importFrom estimatr lm_robust
best_predictor <- function(data, covariate_names, cuts = 20) {
  data.frame(
    inquiry = "best_predictor",
    estimand = lapply(covariate_names, function(j) {
      lm_robust(tau ~ cut(data[[j]], cuts), data = data)$r.squared
    }) %>%
      unlist %>% which.max
  )
}

