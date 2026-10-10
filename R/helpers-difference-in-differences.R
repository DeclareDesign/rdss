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
#' Runs [DIDmultiplegt::did_multiplegt()], the de Chaisemartin and D'Haultfoeuille estimator, and returns its estimate as a one-row data frame.
#'
#' See https://book.declaredesign.org/observational-causal.html#difference-in-differences
#'
#' @param data A data frame, passed to `did_multiplegt()` as `df`.
#' @param ... Further arguments to `did_multiplegt()`, which must include the column names of the outcome (`Y`), group (`G`), time period (`T`), and treatment (`D`), as strings.
#'
#' @return A data frame with one column, `estimate`.
#'
#' @export
#'
#' @importFrom tibble tibble
#'
did_multiplegt_tidy <- function(data, ...) {

  if(!requireNamespace("DIDmultiplegt")){
    message("The did_multiplegt_tidy function requires the 'DIDmultiplegt' package.")
    return(invisible())
  }

  fit <- DIDmultiplegt::did_multiplegt(df = data, ...)
  tibble(estimate = fit$effect)
}
