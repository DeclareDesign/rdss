
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
  group_by_set <- c(diagnosis$group_by_set, "diagnosand")
  if("parameters_df" %in% names(diagnosis)) {
    group_by_set <- c(group_by_set, names(diagnosis$parameters_df))
  }

  diagnosands %>%
    pivot_longer(cols = contains(diagnosand_names),
                 names_to = "diagnosand") %>%
    mutate(is_se = str_detect(diagnosand, "se\\("),
           diagnosand = str_remove(diagnosand, "se\\("),
           diagnosand = str_remove(diagnosand, "\\)")) %>%
    pivot_wider(id_cols = unique(group_by_set), names_from = is_se, values_from = value) %>%
    rename(estimate = `FALSE`, std.error = `TRUE`) %>%
    mutate(conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
}
