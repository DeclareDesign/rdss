#' Tidy estimates from rrreg
#'
#' See https://draft.declaredesign.org/experimental-descriptive.html#list-experiments
#'
#' @param data a formula
#' @param data a data.frame
#'
#' @export
#'
#' @importFrom rr rrreg
#'
rr_forced_known <-
  function(formula, data) {
    fit  <-
      try(rrreg(
        formula,
        data = data,
        p = 2 / 3,
        p0 = 1 / 6,
        p1 = 1 / 6,
        design = "forced-known"
      ))
    pred <-
      try(as.data.frame(predict(fit, avg = TRUE, quasi.bayes = TRUE)))
    if (class(fit) != "try-error" & class(pred) != "try-error") {
      names(pred) <- c("estimate", "std.error", "conf.low", "conf.high")
      pred$p.value <-
        with(pred, 2 * pnorm(-abs(estimate / std.error)))
    } else {
      pred <-
        data.frame(
          estimate = NA,
          std.error = NA,
          conf.low = NA,
          conf.high = NA,
          p.value = NA,
          error = TRUE
        )
    }
    pred
  }
