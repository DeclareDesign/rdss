#' Add parentheses around standard error estimates
#'
#' @param x Numeric vector
#' @param digits Number of digits to retain
#'
#' @return A character vector with enclosing parentheses
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
#' @param x Numeric vector
#' @param digits Number of digits to retain
#'
#' @return a character vector of formatted numbers
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
#' @param estimate a numeric vector of parameter estimates
#' @param std.error a numeric vector of standard error estimates
#' @param digits number of digits to retain
#'
#' @return a character vector of formatted estimates and standard errors
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
#' @param conf.low a numeric vector of lower bounds
#' @param conf.high a numeric vector of upper bounds
#' @param digits number of digits to retain
#'
#' @return a character vector of intervals
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
