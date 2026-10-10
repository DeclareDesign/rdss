#' Tidy helper function for causal_forest function
#'
#' Splits the data at random into a training and a test set, fits [grf::causal_forest()] on the training set, and predicts each unit's treatment effect. The data must contain the outcome as `Y` and the treatment as `Z`.
#'
#' See https://book.declaredesign.org/complex-designs.html#discovery-using-causal-forests
#'
#' @param data A data frame with columns `Y`, `Z`, and the covariates named in `covariate_names`.
#' @param covariate_names A character vector naming the covariate columns the forest may split on.
#' @param share_train The share of units assigned to the training set. Defaults to 0.5.
#' @param ... Further arguments passed to [grf::causal_forest()].
#'
#' @return `data` with four columns added: `pred` (the predicted treatment effect), `var_imp` (the position in `covariate_names` of the most important covariate), and the logical indicators `train` and `test`.
#'
#' @export
#'
#' @importFrom dplyr mutate select all_of `%>%`
#' @importFrom stats quantile predict
#' @importFrom randomizr complete_rs
#'
#' @examplesIf requireNamespace("grf", quietly = TRUE) && requireNamespace("DeclareDesign", quietly = TRUE)
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


#' Find the covariate that best predicts treatment effect heterogeneity
#'
#' An inquiry handler. For each covariate, regresses the true unit-level effect `tau` on the covariate cut into intervals, and reports which covariate gives the highest R-squared. Pair it with [causal_forest_handler()], whose `var_imp` column is the estimate of the same quantity.
#'
#' See https://book.declaredesign.org/complex-designs.html#discovery-using-causal-forests
#'
#' @param data A data frame with a column `tau` holding each unit's treatment effect, and the covariates named in `covariate_names`.
#' @param covariate_names A character vector of covariates to assess.
#' @param cuts Either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which each covariate is to be cut. Defaults to 20.
#'
#' @return A data frame with one row: `inquiry` (`"best_predictor"`) and `estimand`, the position in `covariate_names` of the best predictor.
#'
#' @export
#'
#' @importFrom estimatr lm_robust
#' @examples
#'
#' set.seed(343)
#' dat <- data.frame(A = rnorm(500), B = rnorm(500))
#' dat$tau <- 1 + dat$A
#'
#' # The effect varies with A, so A (position 1) is the best predictor
#' best_predictor(dat, covariate_names = c("A", "B"), cuts = 4)
#'
best_predictor <- function(data, covariate_names, cuts = 20) {
  data.frame(
    inquiry = "best_predictor",
    estimand = lapply(covariate_names, function(j) {
      lm_robust(tau ~ cut(data[[j]], cuts), data = data)$r.squared
    }) %>%
      unlist %>% which.max
  )
}

