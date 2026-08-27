

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
#' The estimator_AS_tidy function requires the 'interference' package, which is not available on CRAN.
#'
#' To use this function, install it with
#' remotes::install_github('szonszein/interference')
#'
#' Without it the function returns nothing and explains why, so a design that
#' includes it still declares and diagnoses.
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param data A `data.frame` containing the observed data.
#'
#' @param permutatation_matrix A matrix of random treatment assignments. Each
#'   column corresponds to one permutation of the treatment vector, as returned
#'   by `t(obtain_permutation_matrix(declaration))`. The exposure probabilities
#'   are computed from it.
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
#' @return a data.frame of estimates
#'
#' @export
#'
#' @importFrom tibble tibble
#'
estimator_AS_tidy <-
  function(data, permutatation_matrix, adj_matrix) {

    if (!requireNamespace("interference", quietly = TRUE)) {
      message(
        "The estimator_AS_tidy function requires the 'interference' package, ",
        "which is not available on CRAN. Install it with ",
        "remotes::install_github('szonszein/interference')"
      )
      return(invisible())
    }

    out_AS <-
      interference::estimates(
        obs_exposure =
          interference::make_exposure_map_AS(
            adj_matrix = adj_matrix,
            tr_vector = data$Z,
            hop = 1
          ),
        obs_outcome = data$Y,
        obs_prob_exposure =
          interference::make_exposure_prob(
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
    )
  }
