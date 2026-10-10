#' Extract mu and tau-squared from a random-effects meta-analysis
#'
#' A tidier for fits from [rma_helper()]: `mu` is the average effect across studies and `tau_sq` the variance of the true effects across studies.
#'
#' See https://book.declaredesign.org/complex-designs.html#meta-analysis
#'
#' @param fit A fit from [rma_helper()] or [metafor::rma()].
#'
#' @return A data frame with two rows, `mu` and `tau_sq`, with estimates, standard errors, and confidence intervals (for `tau_sq`, intervals only when `method = "REML"`). If the fit failed, one row with `estimate = NA` and `error = TRUE`.
#'
#' @export
#'
#' @importFrom broom tidy glance
#' @importFrom dplyr mutate select transmute filter left_join bind_rows
#' @importFrom stats confint
#' @importFrom tibble rownames_to_column
rma_mu_tau <- function(fit) {

  if (!inherits(fit, "try-error")) {
    mu <-
      tidy(fit, conf.int = TRUE) %>%
      mutate(term = "mu") %>%
      select(-type)

    tau <- glance(fit) %>%
      transmute(term = "tau_sq",
                estimate = tau.squared,
                std.error = tau.squared.se)

    if (fit$method == "REML") {
      tau_ci <-
        confint(fit)$random %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        filter(term == "tau^2") %>%
        transmute(term = "tau_sq",
                  conf.low = ci.lb,
                  conf.high = ci.ub)

      tau <- left_join(tau, tau_ci, by = "term")
    }

    bind_rows(mu, tau)

  } else {
    data.frame(estimate = NA, error = TRUE)
  }
}

#' Helper function for rma function in metafor package
#'
#' See https://book.declaredesign.org/complex-designs.html#meta-analysis
#'
#' Fits [metafor::rma()] to study-level estimates and standard errors. If the fit fails, it returns the error as an object [rma_mu_tau()] recognizes, so one failed simulation does not stop a diagnosis.
#'
#' @param data A data frame with one row per study.
#' @param yi The bare column name of the study estimates.
#' @param sei The bare column name of their standard errors.
#' @param method character string to specify whether a fixed- or a random/mixed-effects model should be fitted. A fixed-effects model (with or without moderators) is fitted when using method = "FE". Random/mixed-effects models are fitted by setting method equal to one of the following: "DL", "HE", "SJ", "ML", "REML", "EB", "HS", "HSk", or "GENQ". Default is "REML".
#' @param ... Further arguments to [metafor::rma()].
#'
#' @return An `rma.uni` fit; pass it to [rma_mu_tau()] to tidy it.
#'
#' @export
#'
#' @importFrom rlang quo_text enexpr
rma_helper <- function(data, yi, sei, method = "REML", ...){
  if(!requireNamespace("metafor")){
    message("The rma_helper function requires the 'metafor' package.")
    return(invisible())
  }
  fit <- try({metafor::rma(yi = data[[quo_text(enexpr(yi))]], sei = data[[quo_text(enexpr(sei))]], method = method, ... = ...)})
  if(inherits(fit, "try-error")) {
    class(fit) <- c("rma.uni", "try-error")
  }
  fit
}

