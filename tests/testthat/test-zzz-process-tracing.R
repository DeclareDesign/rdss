
test_that("process_tracing_estimator works", {

  skip_if_not_installed("CausalQueries")
  skip_on_cran()

  library(DeclareDesign)
  library(CausalQueries)

  causal_model <- make_model("X -> M -> Y <- W -> M") |>
    set_restrictions("(M[X=1] < M[X=0]) | (M[X=1, W=1] == M[X=0, W=1])") |>
    set_restrictions("(Y[M=1] < Y[M=0]) | (Y[M=1, W=1] == Y[M=0, W=1])")

  strategies = c("X-Y", "X-Y-M", "X-Y-W",  "X-Y-W-M")

  declaration_16.1 <-
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
      strategies = strategies)

  expect_error(simulate_design(declaration_16.1, sims = 1), NA)

})
