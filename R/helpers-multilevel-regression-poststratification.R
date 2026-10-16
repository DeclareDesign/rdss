#' Post stratification estimator helper
#'
#' Predicts each row's outcome from a fitted model, then averages the predictions within each group using the post-stratification weights. In the book, the rows are demographic cells and the groups are states.
#'
#' See https://book.declaredesign.org/observational-descriptive.html#multi-level-regression-and-poststratification
#'
#' @param model_fit A fitted model that [marginaleffects::predictions()] accepts, e.g. from `lme4::glmer()` or `estimatr::lm_robust()`. For a mixed model, random effects are left out of the predictions (`re.form = NA`); for any other model no such argument is passed.
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
#' @examplesIf requireNamespace("lme4", quietly = TRUE) && requireNamespace("marginaleffects", quietly = TRUE)
#'
#' set.seed(343)
#' # Post-stratification cells: high school graduates and non-graduates in ten
#' # states, weighted by each state's share of graduates
#' cells <- expand.grid(state = state.name[1:10], HS = 0:1)
#' prob_HS <- rep(seq(0.5, 0.9, length.out = 10), 2)
#' cells$PS_weight <- ifelse(cells$HS == 1, prob_HS, 1 - prob_HS)
#'
#' survey <- cells[sample(nrow(cells), 1000, replace = TRUE), ]
#' survey$Y <- rbinom(1000, 1, prob = plogis(0.5 * survey$HS))
#'
#' model_fit <- lme4::glmer(Y ~ HS + (1 | state), data = survey, family = binomial)
#' post_stratification_helper(model_fit, data = cells, group = state, weights = PS_weight)
#'
post_stratification_helper <- function(model_fit, data, group, weights) {

  if(!requireNamespace("marginaleffects")){
    message("The post_stratification_helper function requires the 'marginaleffects' package.")
    return(invisible())
  }

  # `re.form = NA` is an lme4 prediction argument. marginaleffects warns when
  # it is passed to a model that does not take it, so it goes only to a mixed
  # model fit.
  predictions <- if (inherits(model_fit, "merMod")) {
    marginaleffects::predictions(model_fit, newdata = data, type = "response", re.form = NA)
  } else {
    marginaleffects::predictions(model_fit, newdata = data, type = "response")
  }

  predictions %>%
    group_by({{group}}) %>%
    summarize(estimate = weighted.mean(estimate, !!enquo(weights)), .groups = "drop")
}
