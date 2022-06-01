#' Tidy estimates from rrreg
#'
#' See https://draft.declaredesign.org/experimental-descriptive.html#list-experiments
#'
#' @param data a formula
#' @param data a data.frame
#'
#' @export
#'
#' @importFrom rr rrreg
#'
rr_forced_known <-
  function(formula, data) {
    fit  <-
      try(rrreg(
        formula,
        data = data,
        p = 2 / 3,
        p0 = 1 / 6,
        p1 = 1 / 6,
        design = "forced-known"
      ))
    pred <-
      try(as.data.frame(predict(fit, avg = TRUE, quasi.bayes = TRUE)))
    if (class(fit) != "try-error" & class(pred) != "try-error") {
      names(pred) <- c("estimate", "std.error", "conf.low", "conf.high")
      pred$p.value <-
        with(pred, 2 * pnorm(-abs(estimate / std.error)))
    } else {
      pred <-
        data.frame(
          estimate = NA,
          std.error = NA,
          conf.low = NA,
          conf.high = NA,
          p.value = NA,
          error = TRUE
        )
    }
    pred
  }





#' Tidy estimates from the amce estimator
#'
#'
#'
#' @param fit an amce fit object from cjoint::amce
#' @param alpha the type 1 error rate, set to 0.05 by default. Used for the calculation of confidence intervals
#'
#' @return
#' @export
#'
#' @importFrom dplyr `%>%` rename select mutate
#' @importFrom cjoint summary
#'
#' @examples
#'
#'
#' data("immigrationconjoint")
#' data("immigrationdesign")
#' # Run AMCE estimator using all attributes in the design
#' results <- amce(Chosen_Immigrant ~  Gender + Education + `Language Skills` +
#'                   `Country of Origin` + Job + `Job Experience` + `Job Plans` +
#'                   `Reason for Application` + `Prior Entry`, data=immigrationconjoint,
#'                 cluster=TRUE, respondent.id="CaseID", design=immigrationdesign)
#' # Print summary
#' tidy_amce(results)
#'
#'
tidy_amce <-
  function(fit, alpha = 0.05) {
    z_score = qnorm(1 - ((alpha) / 2))
    summary_fit <- summary(fit)
    summary_fit$amce %>%
      rename(
        estimate = Estimate,
        std.error = `Std. Err`,
        statistic = `z value`,
        p.value = `Pr(>|z|)`
      ) %>%
      select(-" ") %>%
      mutate(
        conf.low = estimate - z_score * std.error,
        conf.high = estimate + z_score * std.error,
      )
  }
