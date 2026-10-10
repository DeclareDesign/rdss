#' Tidy helper function for rdrobust function
#'
#' A [generics::tidy()] method for fits from [rdrobust::rdrobust()], so they can be used in `declare_estimator()`. Use [rdrobust_helper()] to fit the model.
#'
#' See https://book.declaredesign.org/observational-causal.html#regression-discontinuity-designs
#'
#' @param x A fit from [rdrobust::rdrobust()].
#' @param ... Not used.
#'
#' @return A data frame with one row per estimate rdrobust reports (conventional, bias-corrected, and robust) and columns `term`, `estimate`, `std.error`, `statistic`, `p.value`, `conf.low`, `conf.high`, and `cutoff`.
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
#' Lets [rdrobust::rdrobust()], which takes vectors, be called with a data frame and column names. Pass it as `.method` in `declare_estimator()`; [tidy.rdrobust()] then tidies the fit.
#'
#' See https://book.declaredesign.org/observational-causal.html#regression-discontinuity-designs
#'
#' @param data A data frame.
#' @param y The bare column name of the outcome.
#' @param x The bare column name of the running variable.
#' @param subset An optional logical expression in terms of the columns of `data`, selecting the rows to use.
#' @param ... Further arguments to [rdrobust::rdrobust()], e.g. `c` for the cutoff.
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

