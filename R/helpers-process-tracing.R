#' Process tracing estimator
#'
#' Answers a causal query about one case using a causal model from 'CausalQueries', once for each process tracing strategy. A strategy names the nodes whose values are observed, so the estimate shows how the answer changes as more of the case's data is examined.
#'
#' See https://book.declaredesign.org/observational-causal.html#process-tracing
#'
#' @param causal_model A model from `CausalQueries::make_model()`.
#' @param data A one-row data frame giving the case's value on each observed node. Nodes that are `NA` are treated as unobserved.
#' @param query The causal query, in 'CausalQueries' syntax, e.g. `"Y[X=1] > Y[X=0]"`.
#' @param strategies A character vector of strategies, each listing the nodes observed, separated by hyphens, e.g. `c("X-Y", "X-M-Y")`.
#'
#' @return A data frame with one row per strategy: `term` (the strategy), `XY` (the case's values of `X` and `Y`, e.g. `"X1Y1"`), and `estimate` (the posterior mean of the query).
#'
#' @export
#' @examplesIf requireNamespace("CausalQueries", quietly = TRUE)
#' # Simple example showing ambiguity in attribution
#' process_tracing_estimator(
#'   causal_model = CausalQueries::make_model("X -> Y"),
#'   query = "Y[X=1] > Y[X=0]",
#'   data = data.frame(X=1, Y = 1),
#'   strategies = "X-Y")
#'
#'# Example where M=1 acts as a hoop test
#' process_tracing_estimator(
#'   causal_model = CausalQueries::make_model("X -> M -> Y") |>
#'    CausalQueries::set_restrictions("Y[M=1] < Y[M=0]") |>
#'    CausalQueries::set_restrictions("M[X=1] < M[X=0]"),
#'   query = "Y[X=1] > Y[X=0]",
#'   data = data.frame(X=1, Y = 1, M = 0),
#'   strategies = c("Y", "X-Y", "X-M-Y"))

process_tracing_estimator <- function(causal_model, query, data, strategies) {

  if(!requireNamespace("CausalQueries")){
    message("The process_tracing_estimator function requires the 'CausalQueries' package.")
    return(invisible())
  }

  if(nrow(data) !=1) {
    stop("please provide a single row dataset")
  }

  given <-
    lapply(strategies, function(x) strsplit(x, "-")[[1]]) |>
    lapply(function(s)
      paste((sapply(s, function(x)
      paste(x, "==", data[x]))[sapply(s, function(x)
        ! is.na(data[x]))]), collapse = " & "))

  causal_model |>
    CausalQueries::query_model(query = query, given = given) |>
    mutate(term = strategies, XY = paste0("X", data$X, "Y", data$Y)) |>
    select(term, XY, estimate = mean)

}



