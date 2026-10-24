

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
#' @examples
#' # An exposure map for three units, one column per exposure condition
#' exposure <- cbind(dir_ind1 = c(1, 0, 0), isol_dir = c(0, 1, 0), no = c(0, 0, 1))
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
#' Runs the Aronow and Samii estimator from the 'interference' package and returns Horvitz-Thompson and Hajek estimates of the total, direct, and indirect effects.
#'
#' The 'interference' package is not available on CRAN, so in the CRAN version of rdss this function only explains how to install the GitHub versions of both packages, where it works:
#' remotes::install_github('szonszein/interference')
#' remotes::install_github('DeclareDesign/rdss')
#'
#' It returns nothing, so a design that includes it still declares and diagnoses.
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param data A data frame with columns `Z` and `Y`.
#' @param permutatation_matrix (The spelling is the published one and is kept so existing code runs.) A matrix of random treatment assignments with one row per permutation of the treatment vector and one column per unit.
#' @param adj_matrix An adjacency matrix defining the network structure.
#' @param obs_prob_exposure Optional precomputed exposure probabilities.
#'
#' @return Nothing, invisibly, in the CRAN version. The GitHub version returns a data frame with columns `term`, `inquiry`, `estimator`, and `estimate`.
#'
#' @export
#'
estimator_AS_tidy <-
  function(data, permutatation_matrix = NULL, adj_matrix, obs_prob_exposure = NULL) {
    message(
      "The estimator_AS_tidy function requires the 'interference' package, ",
      "which is not available on CRAN, and works only in the GitHub version of rdss. ",
      "Install both with remotes::install_github('szonszein/interference') and ",
      "remotes::install_github('DeclareDesign/rdss')"
    )
    return(invisible())
  }
