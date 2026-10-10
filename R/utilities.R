#' Add parentheses around standard error estimates
#'
#' @param x A numeric vector.
#' @param digits The number of decimal places. Defaults to 3.
#'
#' @return A character vector, e.g. `"(0.120)"`.
#'
#' @export
#'
#' @examples
#'
#' std.error <- c(0.12, 0.001, 1.2)
#' add_parens(std.error)
#'
add_parens <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0("(", format_num(x = x, digits = digits), ")"))
}

#' Round and pad a number to a specific decimal place
#'
#' Rounds to `digits` decimal places and keeps trailing zeros, so a column of numbers lines up in a table.
#'
#' @param x A numeric vector.
#' @param digits The number of decimal places. Defaults to 3.
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#'
#' std.error <- c(0.12, 0.001, 1.2)
#' format_num(std.error)
#'
format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

#' Format estimates and standard errors for nice printing
#'
#' @param estimate A numeric vector of estimates.
#' @param std.error A numeric vector of standard errors, the same length as `estimate`.
#' @param digits The number of decimal places. Defaults to 2.
#'
#' @return A character vector, e.g. `"0.07 (0.12)"`.
#'
#' @export
#'
#' @examples
#'
#' estimate <- c(0.07, 0.005, -4)
#' std.error <- c(0.12, 0.001, 1.2)
#'
#' make_se_entry(estimate, std.error)
#'
make_se_entry <- function(estimate, std.error, digits = 2){
  paste0(format_num(estimate, digits = digits)," ", add_parens(std.error, digits = digits))
}

#' Format confidence intervals for nice printing
#'
#' @param conf.low A numeric vector of lower bounds.
#' @param conf.high A numeric vector of upper bounds.
#' @param digits The number of decimal places. Defaults to 2.
#'
#' @return A character vector, e.g. `"[-0.17, 0.31]"`.
#'
#' @export
#'
#' @examples
#'
#' conf.low <- c(-0.1652, 0.00304, -6.352)
#' conf.high <- c(0.3052, 0.00696, -1.648)
#'
#' make_interval_entry(conf.low, conf.high)
#'
#'
make_interval_entry <-
  function(conf.low, conf.high, digits = 2) {
    paste0(
      "[",
      format_num(conf.low, digits = digits),
      ", ",
      format_num(conf.high, digits = digits),
      "]"
    )
  }
