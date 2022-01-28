

#' Tidy results from a stanreg regresion and exponentiate the estimated coefficient
#'
#' Note no standard errors or other summary statistics are provided
#'
#' See https://book.declaredesign.org/choosing-an-answer-strategy.html#bayesian-formalizations
#'
#' @param fit A stanreg fit from stan_glm
#'
#' @importFrom broom.mixed tidy
#'
#' @export
tidy_exponentiate <- function(fit) {
  stopifnot(inherits(fit, "stanreg"))
  tidy_fit <- tidy(fit)
  tidy_fit$estimate <- exp(tidy_fit$estimate)
  tidy_fit$std.error <- NULL
  tidy_fit
}
