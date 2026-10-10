#' Post stratification estimator helper
#'
#' Predicts each row's outcome from a fitted model, then averages the predictions within each group using the post-stratification weights. In the book, the rows are demographic cells and the groups are states.
#'
#' See https://book.declaredesign.org/observational-descriptive.html#multi-level-regression-and-poststratification
#'
#' @param model_fit A fitted model that [marginaleffects::predictions()] accepts, e.g. from `lme4::glmer()`. Random effects are left out of the predictions (`re.form = NA`).
#' @param data A data frame of post-stratification cells to predict for.
#' @param group The bare column name of the grouping variable.
#' @param weights The bare column name of the post-stratification weights, e.g. each cell's population share.
#'
#' @return A data frame with one row per group and columns for the group and `estimate`.
#'
#' @export
#'
#' @importFrom dplyr group_by summarize
#' @importFrom stats weighted.mean
#' @importFrom rlang `!!` enquo
#'
post_stratification_helper <- function(model_fit, data, group, weights) {

  if(!requireNamespace("marginaleffects")){
    message("The post_stratification_helper function requires the 'marginaleffects' package.")
    return(invisible())
  }

  marginaleffects::predictions(
    model_fit,
    newdata = data,
    type = "response",
    re.form = NA
    # For this model type, `marginaleffects` only takes into account the
    # uncertainty in fixed-effect parameters. You can use the `re.form=NA`
    # argument to acknowledge this explicitly and silence this warning.
  ) %>%
    group_by({{group}}) %>%
    summarize(estimate = weighted.mean(estimate, !!enquo(weights)), .groups = "drop")
}
