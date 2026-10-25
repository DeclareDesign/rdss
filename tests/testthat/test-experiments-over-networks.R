
test_that("interference helpers work", {

  skip_if_not_installed("spdep")
  skip_if_not_installed("interference") # https://github.com/szonszein/interference
  skip_on_cran()

  library(dplyr)
  library(DeclareDesign)
  library(spdep)
  library(interference)

  # Here we obtain the adjacency matrix
  adj_matrix <-
    fairfax |>
    as("Spatial") |>
    poly2nb(queen = TRUE) |>
    nb2mat(style = "B", zero.policy = TRUE)

  # Here we create a permutation matrix of possible random assignments
  ra_declaration <- declare_ra(N = 238, prob = 0.1)

  permutatation_matrix <-
    ra_declaration |>
    obtain_permutation_matrix(maximum_permutations = 10000) |>
    t()

  declaration_18.13 <-
    declare_model(
      data = select(as_tibble(fairfax), -geometry),
      Y_0_0 = pnorm(scale(SHAPE_LEN), sd = 3),
      Y_1_0 = Y_0_0 + 0.02,
      Y_0_1 = Y_0_0 + 0.01,
      Y_1_1 = Y_0_0 + 0.03
    ) +
    declare_inquiry(
      total_ATE = mean(Y_1_1 - Y_0_0),
      direct_ATE = mean(Y_1_0 - Y_0_0),
      indirect_ATE = mean(Y_0_1 - Y_0_0)
    ) +
    declare_assignment(
      Z = conduct_ra(ra_declaration),
      exposure = get_exposure_AS(make_exposure_map_AS(adj_matrix, Z, hop = 1))
    ) +
    declare_measurement(
      Y = case_when(
        exposure == "dir_ind1" ~ Y_1_1,
        exposure == "isol_dir" ~ Y_1_0,
        exposure == "ind1" ~ Y_0_1,
        exposure == "no" ~ Y_0_0
      )
    ) +
    declare_estimator(handler = estimator_AS_tidy,
                      permutatation_matrix = permutatation_matrix,
                      adj_matrix = adj_matrix)

  expect_error(simulate_once(declaration_18.13), NA)

  # Precomputed exposure probabilities give the same estimates as computing them
  # from the permutation matrix
  prob_exposure <-
    make_exposure_prob(
      potential_tr_vector = permutatation_matrix,
      adj_matrix = adj_matrix,
      exposure_map_fn = make_exposure_map_AS,
      exposure_map_fn_add_args = list(hop = 1)
    )
  set.seed(343)
  dat <- draw_data(declaration_18.13)
  set.seed(343)
  from_matrix <- estimator_AS_tidy(dat, permutatation_matrix = permutatation_matrix, adj_matrix = adj_matrix)
  set.seed(343)
  precomputed <- estimator_AS_tidy(dat, adj_matrix = adj_matrix, obs_prob_exposure = prob_exposure)
  expect_equal(precomputed, from_matrix)

  # Standard errors come from contrasts with the "no" condition; without
  # control_condition, interference::estimates() returns empty variances
  expect_equal(from_matrix$term, rep(c("dir_ind1", "isol_dir", "ind1"), 2))
  expect_true(all(from_matrix$std.error > 0))
  expect_true(all(from_matrix$conf.low < from_matrix$estimate & from_matrix$estimate < from_matrix$conf.high))

  expect_error(estimator_AS_tidy(dat, adj_matrix = adj_matrix), "Supply either")

})
