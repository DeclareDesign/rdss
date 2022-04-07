#' Process tracing estimator
#'
#' Draw conclusions from a model given a query, data, and process tracing strategies
#' See https://book.declaredesign.org/observational-causal.html#process-tracing
#'
#' @param causal_model a model generation by `CausalQueries`
#' @param data a single row dataset with data on nodes in the model
#' @param query a causal query of interest
#' @param strategies a list of sets of nodes examined
#'
#' @export
#'
#' @importFrom CausalQueries query_model
#'
pt_estimator <- function(causal_model, query, data, strategies)

  causal_model %>%

  query_model(query = query,
              given = strategy_statements(data, strategies)) %>%

  select(estimate = mean) %>%

  mutate(XY = paste0("X", data$X, "Y", data$Y),
         term = strategies %>% lapply(paste, collapse = "-") %>% unlist)



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

#' Tidy estimates from did_multiplegt
#'
#' See https://book.declaredesign.org/observational-causal.html#difference-in-differences
#'
#' @param data a data.frame
#' @param ... options passed to did_multiplegt
#'
#' @export
#'
#' @importFrom DIDmultiplegt did_multiplegt
#' @importFrom tibble tibble
#'
did_multiplegt_tidy <- function(data, ...) {
  fit <- did_multiplegt(df = data, ...)
  tibble(estimate = fit$effect)
}
