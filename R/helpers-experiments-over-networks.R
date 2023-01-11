

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
#' The estimator_AS_tidy function requires the 'interference' package, which is not yet available on CRAN.
#'
#' To use this function:
#' 1) install the developer version of interference via remotes::install_github('szonszein/interference') and
#' 2) install the developer version of rdss via remotes::install_github('DeclareDesign/rdss@remotes')
#'
#' See https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
#'
#' @param data a data.frame
#' @param permutatation_matrix a permutation matrix of random assignments
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
    message("The estimator_AS_tidy function requires the 'interference' package, which is not yet available on CRAN. To use this function, install the developer version of interference via remotes::install_github('szonszein/interference') and then install the developer version of rdss via remotes::install_github('DeclareDesign/rdss@remotes')")
    return(invisible())
  }
