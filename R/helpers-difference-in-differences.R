#' Generate lags in grouped data
#'
#' Lags `x` within each group, ordering by `order_by` rather than by row order. Write the variables as bare column names inside [fabricatr::fabricate()] or [dplyr::mutate()]; see the README for an example.
#'
#' See https://book.declaredesign.org/observational-causal.html#difference-in-differences
#'
#' @param x The variable to lag.
#' @param groups The grouping variable, e.g. a unit identifier.
#' @param n A positive integer: the number of periods to lag by. Defaults to 1.
#' @param order_by Ordering variable within group (e.g., time)
#' @param default The value for rows with no earlier period in their group. Defaults to `NA`.
#'
#' @return A vector the length of `x`, in the original row order.
#'
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom rlang enexpr `:=` `!!`
#' @importFrom dplyr group_by mutate ungroup pull lag `%>%`
#'
#'
#' @examples
#'
#' # Three units observed in four periods; unit u is treated after period u
#' panel <- expand.grid(unit = 1:3, period = 1:4)
#' panel$D <- as.numeric(panel$period > panel$unit)
#'
#' dplyr::mutate(panel, D_lag = lag_by_group(D, groups = unit, order_by = period))
#'
lag_by_group <- function(x, groups, n = 1, order_by, default = NA) {
  x_nm <- enexpr(x)
  grp_nm <- enexpr(groups)
  tibble(!!x_nm := x, !!grp_nm := groups, order_by___ = order_by) %>%
    group_by(!!grp_nm) %>%
    mutate(!!x_nm := lag(!!x_nm, n = n, default = default, order_by = order_by___)) %>%
    ungroup %>%
    pull(!!x_nm)
}

#' Tidy helper function for did_multiplegt
#'
#' Runs [DIDmultiplegt::did_multiplegt()], the de Chaisemartin and D'Haultfoeuille estimator, and returns its estimates as a data frame.
#'
#' Use `mode = "dyn"`. It estimates the effect `l` periods after a group first switches treatment, for `l` from 1 to `effects`; `effects = 1` is the effect in the period of the switch. This mode runs `did_multiplegt_dyn()` from the 'DIDmultiplegtDYN' package, which also needs the 'polars' package. polars is not on CRAN; install it with
#' `install.packages("polars", repos = "https://rpolars.r-universe.dev")`
#' and attach it with `library(polars)`.
#'
#' `mode = "old"` is still passed through, but in 'DIDmultiplegt' 2.1.0 it returns `NaN`: it takes first differences with `stats::lag()`, which does not shift the data, so no group is ever seen to switch. The function warns when that happens.
#'
#' See https://book.declaredesign.org/observational-causal.html#difference-in-differences
#'
#' @param data A data frame, passed to `did_multiplegt()` as `df`.
#' @param ... Further arguments to `did_multiplegt()`. These must include `mode` and, for `mode = "dyn"`, the column names of the outcome (`outcome`), group (`group`), time period (`time`), and treatment (`treatment`), as strings, and the number of effects to estimate (`effects`). `graph_off` defaults to `TRUE`, so that no plot is drawn.
#'
#' @return For `mode = "dyn"`, a data frame with one row per effect and columns `term` (`"Effect_1"`, `"Effect_2"`, ...), `estimate`, `std.error`, `conf.low`, `conf.high`, `n` (the number of group-period cells used), and `n_switchers` (the number of switching groups). For `mode = "old"`, a data frame with one column, `estimate`.
#'
#' @export
#'
#' @importFrom tibble tibble
#'
#' @examplesIf requireNamespace("DIDmultiplegt", quietly = TRUE) && requireNamespace("polars", quietly = TRUE)
#'
#' library(polars)
#'
#' set.seed(343)
#' # 20 units in 10 periods; units switch into treatment at different times
#' panel <- data.frame(unit = rep(1:20, times = 10), period = rep(1:10, each = 20))
#' panel$D <- as.numeric(panel$period > (panel$unit %% 5) + 4)
#' panel$Y <- 0.5 * panel$D + rnorm(nrow(panel))
#'
#' did_multiplegt_tidy(
#'   panel, mode = "dyn",
#'   outcome = "Y", group = "unit", time = "period", treatment = "D",
#'   effects = 2
#' )
#'
did_multiplegt_tidy <- function(data, ...) {

  if(!requireNamespace("DIDmultiplegt")){
    message("The did_multiplegt_tidy function requires the 'DIDmultiplegt' package.")
    return(invisible())
  }

  args <- list(...)
  if (identical(args$mode, "dyn") && is.null(args$graph_off)) {
    args$graph_off <- TRUE
  }

  fit <- do.call(DIDmultiplegt::did_multiplegt, c(list(df = data), args))

  if (inherits(fit, "did_multiplegt_dyn")) {
    effects <- fit$results$Effects
    return(tibble(
      term = rownames(effects),
      estimate = effects[, "Estimate"],
      std.error = effects[, "SE"],
      conf.low = effects[, "LB CI"],
      conf.high = effects[, "UB CI"],
      n = effects[, "N"],
      n_switchers = effects[, "Switchers"]
    ))
  }

  if (is.nan(fit$effect)) {
    warning(
      "did_multiplegt() returned NaN. Its old mode does not detect treatment ",
      "switches in DIDmultiplegt 2.1.0; use mode = \"dyn\".",
      call. = FALSE
    )
  }
  tibble(estimate = fit$effect)
}
