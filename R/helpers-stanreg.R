

#' Tidy results from a stanreg regression (deprecated)
#'
#' Deprecated: use `broom.mixed::tidy()`, which this function now calls with the same arguments.
#'
#' See https://book.declaredesign.org/choosing-an-answer-strategy.html#bayesian-formalizations
#'
#' @param x A fit from `rstanarm::stan_glm()`.
#' @param conf.int Logical indicating whether or not to include a confidence interval in the tidied output. Defaults to FALSE.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param exponentiate Logical indicating whether or not to exponentiate the coefficient estimates. Defaults to FALSE.
#' @param ... Further arguments to `broom.mixed::tidy()`.
#'
#' @return A data frame with one row per coefficient.
#'
#' @export
#' @examplesIf requireNamespace("rstanarm", quietly = TRUE) && requireNamespace("broom.mixed", quietly = TRUE)
#'
#' \donttest{
#' fit <- rstanarm::stan_glm(mpg ~ wt, data = mtcars, chains = 1, iter = 1000,
#'                           refresh = 0, seed = 343)
#'
#' # Prefer broom.mixed::tidy(fit, conf.int = TRUE)
#' tidy_stan(fit, conf.int = TRUE)
#' }
#'
tidy_stan <- function(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...) {

  message("This function is deprecated. Please use the 'tidy' function from the 'broom.mixed' package.")

  if(!requireNamespace("broom.mixed")){
    message("The tidy_stan function requires the 'broom.mixed' package.")
    return(invisible())
  }

  ret <- broom.mixed::tidy(x, conf.int = conf.int, conf.level = conf.level, exponentiate = exponentiate, ...)

  ret
}
