print('declaration_15.1.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


# remotes::install_github("macartan/CausalQueries")
library(CausalQueries)
causal_model <- make_model("X -> M -> Y <- W -> M") |>
  set_restrictions("(M[X=1] < M[X=0]) | (M[X=1, W=1] == M[X=0, W=1])") |>
  set_restrictions("(Y[M=1] < Y[M=0]) | (Y[M=1, W=1] == Y[M=0, W=1])")
pt_strategies = list(
  c("X", "Y"),
  c("X", "Y", "M"),
  c("X", "Y", "W"),
  c("X", "Y", "W", "M"))
declaration_15.1 <-
  declare_model(draw_causal_type(causal_model)) +
  declare_inquiry(
    CoE =  query_distribution(
      causal_model, 
      query = "Y[X=1] - Y[X=0]", 
      parameters = causal_type)) +
  declare_measurement(
    handler = function(data)
      causal_model |>
      make_data(parameters = data$causal_type))  +
  declare_estimator(
    handler = label_estimator(process_tracing_estimator), 
    causal_model = causal_model,
    query = "Y[X=1] - Y[X=0]",
    strategies = pt_strategies)
