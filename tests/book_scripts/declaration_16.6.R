print('declaration_16.6.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


returned <- function(x1, a_2 = 1 / 3) {
  ((2 * a_2 * x1 - (1 - a_2) * (1 - x1)) / (2 * x1)) * (x1  > (1 - a_2) / (1 + a_2))
}
invested <- function(a_1, a_2) {
  u_a = (1 - a_1) * log(1 - a_1) + a_1 * log(2 * a_1)  # give a1
  u_b = (1 - a_1) * log(2 * a_2) + a_1 * log(2 * (1 - a_2)) # give 1
  ifelse(u_a > u_b, a_1, 1)
}
average_invested <- function(a_1) {
  mean(sapply(seq(0, 1, .01),  invested, a_1 = a_1))
}
average_returned <- function(a_2) {
  mean(sapply(seq(0.01, 1, .01), returned, a_2 = a_2))
}
rho     <- 0.8
n_pairs <- 200
declaration_16.6 <-
  declare_model(N = 2 * n_pairs,
                a = runif(N),
                arrival = rank(correlate(given = a, rho = rho, runif))) +
  declare_inquiries(
    mean_invested = mean(sapply(a, average_invested)),
    mean_returned = mean(sapply(a, average_returned)),
    return_from_1 = mean(returned(1, a))
  ) +
  declare_assignment(pair = (arrival - 1) %% n_pairs,
                     role = 1 + (arrival > n_pairs)) +
  declare_step(
    id_cols = pair,
    names_from = role,
    values_from = c(ID, a),
    handler = tidyr::pivot_wider
  ) +
  declare_measurement(invested = invested(a_1, a_2),
                      returned = returned(invested, a_2)) +
  declare_estimator(invested ~ 1,
                    method = lm_robust,
                    inquiry = "mean_invested",
                    label = "mean_invested") +
  declare_estimator(returned ~ 1,
                    method = lm_robust,
                    inquiry = "mean_returned",
                    label = "mean_returned") +
  declare_estimator(
    returned ~ 1,
    method = lm_robust,
    subset = invested == 1,
    inquiry = "return_from_1",
    label = "return_from_1"
  )
