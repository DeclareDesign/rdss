
#' Title
#'
#' @param diagnosis
#'
#' @return
#' @export
#'
#' @examples
tidy_diagnosis <- function(diagnosis) {
  diagnosands <- get_diagnosands(diagnosis)
  diagnosand_names <- diagnosis$diagnosand_names

  diagnosands %>%
    pivot_longer(cols = contains(diagnosand_names),
                 names_to = "diagnosand") %>%
    mutate(is_se = str_detect(diagnosand, "se\\("),
           diagnosand = str_remove(diagnosand, "se\\("),
           diagnosand = str_remove(diagnosand, "\\)")) %>%
    pivot_wider(id_cols = c(design, n_x1, inquiry, estimator, term, diagnosand), names_from = is_se, values_from = value) %>%
    rename(estimate = `FALSE`, std.error = `TRUE`) %>%
    mutate(conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
}
