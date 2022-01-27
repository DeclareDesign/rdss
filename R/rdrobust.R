#' Tidy helper function for rdrobust function
#'
#' Runs rdrobust estimation function and returns tidy data frame output
#'
#' See https://book.declaredesign.org/observational-causal.html#regression-discontinuity-designs
#'
#' @param data a data.frame
#' @param y is the dependent variable (rdrobust argument)
#' @param x is the running variable (a.k.a. score or forcing variable) (rdrobust argument)
#' @param c specifies the RD cutoff in x; default is c = 0 (rdrobust argument)
#' @param subset An optional bare (unquoted) expression specifying a subset of observations to be used
#' @param term Symbols or literal character vector of term that represent quantities of interest, i.e. Z. If FALSE, return the first non-intercept term; if TRUE return all term. To escape non-standard-evaluation use !!.
#'
#' @importFrom rlang quo_is_null quo
#' @importFrom dplyr filter pull
#' @importFrom rlang quo_is_null quo enquo
#' @importFrom rdrobust rdrobust
#' @export
rdrobust_helper <-
  function(data, y, x, c, subset = NULL, term = NULL){
    if(!missing(subset))
      data <- filter(data, !!enquo(subset))
    fit <- try(rdrobust(y = pull(data, {{y}}), x = pull(data, {{x}}), c = c))
    if(!inherits(fit, "try-error")) {
      ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci, c = fit$c)
      row.names(ret) <- NULL
      names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "cutoff")
      if(!is.null(term))
        ret <- ret[ret$term == term, ]
    } else {
      ret <- data.frame(term = "Robust", estimate = NA, std.error = NA, statistic = NA, p.value = NA, conf.low = NA, conf.high = NA, cutoff = 0.5, error = as.character(fit))
    }
    ret
  }
