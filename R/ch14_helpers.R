#' Post stratification estimator helper
#'
#' Calculates predicted values from a multilevel regression and the post-stratified state-level estimates
#'
#' Please see https://book.declaredesign.org/observational-descriptive.html#multi-level-regression-and-poststratification
#'
#' @param model_fit a model fit object from, e.g., glmer or lm_robust
#' @param data a data.frame
#'
#' @export
#'
#' @importFrom prediction prediction
#' @importFrom dplyr group_by summarize
#' @importFrom stats weighted.mean
#'
post_stratification_helper <- function(model_fit, data) {
  prediction(
    model_fit,
    data = data,
    allow.new.levels = TRUE,
    type = "response"
  ) %>%
    group_by(state) %>%
    summarize(estimate = weighted.mean(fitted, PS_weight))
}
