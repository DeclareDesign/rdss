#' Extract mu and tau parameters from rma model fit
#'
#' See https://book.declaredesign.org/complex-designs.html#meta-analysis
#'
#' @param fit Fit object from the rma function in the metafor package
#'
#' @return a data.frame of estimates
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
#' See ?rma for further details
#'
#' @param data a data.frame
#' @param yi unquoted variable name of estimates used in meta-analysis
#' @param sei unquoted variable name of standard errors used in meta-analysis
#' @param method character string to specify whether a fixed- or a random/mixed-effects model should be fitted. A fixed-effects model (with or without moderators) is fitted when using method = "FE". Random/mixed-effects models are fitted by setting method equal to one of the following: "DL", "HE", "SJ", "ML", "REML", "EB", "HS", "HSk", or "GENQ". Default is "REML".
#' @param ... Further options to be passed to rma
#'
#' @return a data.frame of estimates
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

