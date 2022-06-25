#' Tidy helper function for rdrobust function
#'
#' Runs rdrobust estimation function and returns tidy data frame output
#'
#' See https://book.declaredesign.org/observational-causal.html#regression-discontinuity-designs
#'
#' @param x Model fit object from rdrobust
#' @param ... Other arguments (not used)
#'
#' @return a data.frame of estimates
#'
#' @export
tidy.rdrobust <- function(x, ...) {
  if(!requireNamespace("rdrobust")){
    message("The rdrobust_tidy function requires the 'rdrobust' package.")
    return(invisible())
  }

  ret <- data.frame(rownames(x$coef), x$coef, x$se, x$z, x$pv, x$ci, c = x$c)
  row.names(ret) <- NULL
  names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "cutoff")

  ret
}


#' Helper function for using rdrobust as a model in `declare_estimator`
#'
#' @param data a data.frame
#' @param y unquoted name of the outcome variable
#' @param x unquoted name of the running variable
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process
#' @param ... Other arguments to \code{rdrobust}
#'
#' @importFrom dplyr filter
#' @importFrom rlang enquo `!!`
#'
#' @return rdrobust model fit object
#'
#' @export
rdrobust_helper <- function(data, y, x, subset = NULL, ...) {
  if(!missing(subset))
    data <- filter(data, !!enquo(subset))
  rdrobust::rdrobust(y = pull(data, {{y}}), x = pull(data, {{x}}), ... = ...)
}

