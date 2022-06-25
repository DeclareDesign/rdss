#' Generate lags in grouped data
#'
#' See https://book.declaredesign.org/observational-causal.html#difference-in-differences
#'
#' @param x Vector of values
#' @param groups Grouping variable
#' @param n Positive integer of length 1, giving the number of positions to lead or lag by
#' @param order_by Ordering variable withing group (e.g., time)
#' @param default Value used for non-existent rows. Defaults to NA.
#'
#' @return vector of lagged values
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
#' Runs did_multiplegt estimation function and returns tidy data frame output
#'
#' See https://book.declaredesign.org/observational-causal.html#difference-in-differences
#'
#' @param data a data.frame
#' @param ... options passed to did_multiplegt
#'
#' @return a data.frame of estimates
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
