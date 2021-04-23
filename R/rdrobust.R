
#' @export
rdrobust_helper <-
  function(data, y, x, c, subset, term){
    data <- filter(data, !!enquo(subset))
    fit <- try(rdrobust(y = pull(data, {{y}}), x = pull(data, {{x}}), c = c))
    if(!inherits(fit, "try-error")) {
      ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci, c = fit$c)
      row.names(ret) <- NULL
      names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "cutoff")
      ret <- ret[ret$term == term, ]
    } else {
      ret <- data.frame(term = "Robust", estimate = NA, std.error = NA, statistic = NA, p.value = NA, conf.low = NA, conf.high = NA, cutoff = 0.5, error = as.character(fit))
    }
    ret
  }
