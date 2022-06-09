

#' Tidy results from a stanreg regresion and exponentiate the estimated coefficient
#'
#' Note no standard errors or other summary statistics are provided
#'
#' See https://book.declaredesign.org/choosing-an-answer-strategy.html#bayesian-formalizations
#'
#' @param fit A stanreg fit from stan_glm
#'
#' @return data.frame of results
#'
#' @export
tidy.stanreg <- function(fit, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...) {
  if(!requireNamespace("broom.mixed")){
    message("The tidy_exponentiate function requires the 'broom.mixed' package.")
    return(invisible())
  }

  ret <- broom.mixed::tidy(fit, conf.int = conf.int, conf.level = conf.level, ...)

  if (exponentiate) {
    ret <- broom:::exponentiate(ret)
    ret$std.error <- NULL
  }

  ret
}
