
#' Dip CEF function
#'
#' See Ch. 11 https://book.declaredesign.org/redesign.html
#'
#' @param x A numeric vector
#'
#' @export
dip <- function(x) {
  (x <= 1) * x + (x > 1) * (x - 2) ^ 2 + .2
}

#' Conditional expectation function inquiry
#'
#' See Ch. 11 https://book.declaredesign.org/redesign.html
#'
#' @param data A data.frame
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr transmute `%>%`
cef_inquiry <-
  function(data) {
    tibble(X = seq( from = 0, to = 3, length.out = 50)) %>%
      transmute(inquiry = paste0("X_", X),
                estimand = dip(X))
  }

#' Conditional expectation function estimator
#'
#' See Ch. 11 https://book.declaredesign.org/redesign.html
#'
#' @param data A data.frame
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr transmute `%>%`
#' @importFrom tidyr pivot_longer
#' @importFrom stats lm predict
cef_estimator <-
  function(data) {
    new_data <-
      tibble(X = seq(
        from = 0,
        to = 3,
        length.out = 50
      ))

    lm_1 <- lm(Y ~ poly(X, 1), data = data)
    lm_2 <- lm(Y ~ poly(X, 2), data = data)
    lm_3 <- lm(Y ~ poly(X, 3), data = data)
    lm_4 <- lm(Y ~ poly(X, 4), data = data)
    lm_5 <- lm(Y ~ poly(X, 5), data = data)
    lm_6 <- lm(Y ~ poly(X, 6), data = data)

    new_data %>%
      transmute(
        inquiry = paste0("X_", X),
        A1 = predict(lm_1, newdata = new_data),
        A2 = predict(lm_2, newdata = new_data),
        A3 = predict(lm_3, newdata = new_data),
        A4 = predict(lm_4, newdata = new_data),
        A5 = predict(lm_5, newdata = new_data),
        A6 = predict(lm_6, newdata = new_data),
      ) %>%
      pivot_longer(cols = c("A1", "A2", "A3", "A4", "A5", "A6"),
                   names_to = "estimator",
                   values_to = "estimate")
  }
