

#' Helper function to obtain the observed exposure for the Aronow and Samii estimator
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param obs_exposure A numeric vector
#'
#' @return a data.frame of observed exposure to a treatment created using the interference package
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
#'
#' The estimator_AS_tidy function requires the 'interference' package, which is not yet available on CRAN.
#'
#' To use this function:
#' 1) install the developer version of interference via remotes::install_github('szonszein/interference') and
#' 2) install the developer version of rdss via remotes::install_github('DeclareDesign/rdss@remotes')
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param data A `data.frame` containing the observed data.
#'
#' @param p_matrix A matrix of random treatment assignments. Each row
#'   corresponds to one permutation of the treatment vector.
#'
#' @param adj_matrix An adjacency matrix defining the network structure. This
#'   can be created, for example, as follows:
#'   \preformatted{
#'   adjacency <- fairfax |>
#'     as("Spatial") |>
#'     spdep::poly2nb(queen = TRUE) |>
#'     spdep::nb2mat(style = "B", zero.policy = TRUE)
#'   }
#'
#' @param obs_prob_exposure A set of exposure probabilities. These can be
#'   generated, for example, using:
#'   \preformatted{
#'   prob_exposure <- interference::make_exposure_prob(
#'     potential_tr_vector = permutations,
#'     adj_matrix = adjacency,
#'     exposure_map_fn = interference::make_exposure_map_AS,
#'     exposure_map_fn_add_args = list(hop = 1)
#'   )
#'   }
#'
#' @return a data.frame of estimates
#'
#' @export
#'
#' @importFrom tibble tibble
#'
estimator_AS_tidy <-
  function(data, p_matrix, adj_matrix, obs_prob_exposure) {

    obs_exposure <-
      interference::make_exposure_map_AS(
        adj_matrix = adj_matrix,
        tr_vector = data$Z,
        hop = 1
      )

    out_AS <-
      interference::estimates(
        obs_exposure = obs_exposure,
        obs_outcome = data$Y,
        obs_prob_exposure = obs_prob_exposure,
        n_var_permutations = 30
      )

    tibble(
      term = c(names(out_AS$tau_ht), names(out_AS$tau_h)),
      inquiry = rep(c("total_ATE", "direct_ATE", "indirect_ATE"), 2),
      estimator = rep(c("Horvitz-Thompson", "Hajek"), each = 3),
      estimate = c(out_AS$tau_ht, out_AS$tau_h)
    )
  }

