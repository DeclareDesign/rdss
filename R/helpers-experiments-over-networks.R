

#' Helper function to obtain the observed exposure for the Aronow and Samii estimator
#'
#' Converts the exposure map from `interference::make_exposure_map_AS()`, a 0/1 matrix with one row per unit and one column per exposure condition, into a vector naming each unit's condition (e.g. `"dir_ind1"` or `"no"`).
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param obs_exposure An exposure map, as returned by `interference::make_exposure_map_AS()`.
#'
#' @return A character vector with one element per unit.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull everything
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
#' @examplesIf requireNamespace("interference", quietly = TRUE)
#'
#' set.seed(343)
#' # A ring of 20 units, each adjacent to the units on either side of it
#' N <- 20
#' adjacency <- matrix(0, N, N)
#' adjacency[cbind(1:N, c(2:N, 1))] <- 1
#' adjacency <- adjacency + t(adjacency)
#' Z <- randomizr::complete_ra(N)
#'
#' exposure <- interference::make_exposure_map_AS(
#'   adj_matrix = adjacency, tr_vector = Z, hop = 1
#' )
#' get_exposure_AS(exposure)
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
#' Runs the Aronow and Samii estimator from the 'interference' package and returns Horvitz-Thompson and Hajek estimates of the total, direct, and indirect effects. Exposure is defined by one-hop neighbours in `adj_matrix`. The data must contain the treatment as `Z` and the outcome as `Y`.
#'
#' The function requires the 'interference' package, which is not available on CRAN.
#'
#' To use this function, install it with
#' remotes::install_github('szonszein/interference')
#'
#' Without it the function returns nothing and explains why, so a design that
#' includes it still declares and diagnoses.
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param data A data frame with columns `Z` and `Y`.
#'
#' @param permutatation_matrix (The spelling is the published one and is kept so existing code runs.) A matrix of random treatment assignments with one
#'   row per permutation of the treatment vector and one column per unit, as returned
#'   by `t(obtain_permutation_matrix(declaration))`. The exposure probabilities
#'   are computed from it unless `obs_prob_exposure` is supplied.
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
#' @param obs_prob_exposure Optional exposure probabilities. They depend only
#'   on the permutation matrix and the network, so in a simulation they are the
#'   same on every draw; computing them once and passing them here avoids
#'   recomputing them each time. For example:
#'   \preformatted{
#'   prob_exposure <- interference::make_exposure_prob(
#'     potential_tr_vector = permutatation_matrix,
#'     adj_matrix = adjacency,
#'     exposure_map_fn = interference::make_exposure_map_AS,
#'     exposure_map_fn_add_args = list(hop = 1)
#'   )
#'   }
#'   When `NULL`, the default, they are computed from `permutatation_matrix`.
#'
#' @return A data frame with six rows (three inquiries by two estimators) and columns `term`, `inquiry`, `estimator`, and `estimate`. No standard errors are returned.
#'
#' @export
#'
#' @importFrom tibble tibble
#'
#' @examplesIf requireNamespace("interference", quietly = TRUE)
#'
#' set.seed(343)
#' # A ring of 20 units, each adjacent to the units on either side of it
#' N <- 20
#' adjacency <- matrix(0, N, N)
#' adjacency[cbind(1:N, c(2:N, 1))] <- 1
#' adjacency <- adjacency + t(adjacency)
#' Z <- randomizr::complete_ra(N)
#'
#' # 100 possible assignments, one per row
#' permutations <- t(replicate(100, randomizr::complete_ra(N)))
#'
#' dat <- data.frame(Z = Z, Y = Z + rnorm(N))
#' estimator_AS_tidy(dat, permutatation_matrix = permutations, adj_matrix = adjacency)
#'
estimator_AS_tidy <-
  function(data, permutatation_matrix = NULL, adj_matrix, obs_prob_exposure = NULL) {

    if (!requireNamespace("interference", quietly = TRUE)) {
      message(
        "The estimator_AS_tidy function requires the 'interference' package, ",
        "which is not available on CRAN. Install it with ",
        "remotes::install_github('szonszein/interference')"
      )
      return(invisible())
    }

    if (is.null(obs_prob_exposure)) {
      if (is.null(permutatation_matrix)) {
        stop("Supply either `permutatation_matrix` or `obs_prob_exposure`.", call. = FALSE)
      }
      obs_prob_exposure <-
        interference::make_exposure_prob(
          potential_tr_vector = permutatation_matrix,
          adj_matrix = adj_matrix,
          exposure_map_fn = interference::make_exposure_map_AS,
          exposure_map_fn_add_args = list(hop = 1)
        )
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
