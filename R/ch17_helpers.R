

#' Helper function to obtain the observed exposure for the Aronow and Samii estimator
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param obs_exposure A numeric vector
#'
#' @return a data.frame of observed exposure to a treatment
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull everything
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
get_exposure_AS <- function(obs_exposure) {
  obs_exposure %>%
    as_tibble() %>%
    pivot_longer(everything()) %>%
    filter(value == 1) %>%
    pull(name)
}

#' Tidy helper function for estimator_AS function
#'
#' Runs estimates estimation function from interference package and returns tidy data frame output
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param data a data.frame
#' @param permutatation_matrix a permuatation matrix of random assignments
#' @param adj_matrix an adjacency matrix
#'
#' @return a data.frame of estimates
#'
#' @export
#'
#' @importFrom tibble tibble
#'
estimator_AS_tidy <-
  function(data, permutatation_matrix, adj_matrix) {
    out_AS <-
      interference::estimates(
        obs_exposure =
          interference::make_exposure_map_AS(adj_matrix = adj_matrix, tr_vector = data$Z, hop = 1),
        obs_outcome = data$Y,
        obs_prob_exposure = interference::make_exposure_prob(
          potential_tr_vector = permutatation_matrix,
          adj_matrix = adj_matrix,
          exposure_map_fn = interference::make_exposure_map_AS,
          exposure_map_fn_add_args = list(hop = 1)
        ),
        n_var_permutations = 30
      )
    tibble(
      term = c(names(out_AS$tau_ht), names(out_AS$tau_h)),
      inquiry = rep(c("total_ATE", "direct_ATE", "indirect_ATE"), 2),
      estimator = rep(c("Horvitz-Thompson", "Hajek"), each = 3),
      estimate = c(out_AS$tau_ht, out_AS$tau_h)
      # something appears to have changed in the interference package;
      # for the moment, only returning estimates, not variance estimates (2022-10-30)
      #  variance = c(out_AS$var_tau_ht, out_AS$var_tau_h),
      #  std.error = sqrt(variance),
      #  conf.low = estimate - 1.96 * std.error,
      #  conf.high = estimate + 1.96 * std.error
    )
  }
