#' Tidy helper function for causal_forest function
#'
#' Runs estimates estimation function from interference package and returns tidy data frame output
#'
#' https://draft.declaredesign.org/complex-designs.html#discovery-using-causal-forests
#'
#' See ?causal_forest for further details
#'
#' @param data A data.frame
#' @param covariate_names Names of covariates
#' @param share_train Share of units to be used for training
#' @param ... Options to causal_forest
#'
#' @return a data.frame of estimates
#'
#' @export
#'
#' @importFrom dplyr mutate select all_of `%>%`
#' @importFrom stats quantile
#'
#' @examples
#'
#' fabricate(
#'    N = 1000,
#'    A = rnorm(N),
#'    B = rnorm(N),
#'    Z = complete_rs(N),
#'    Y = A*Z + rnorm(N)) %>%
#'  causal_forest_handler(covariate_names = c("A", "B")) %>%
#'  ggplot(aes(A, pred)) + geom_point()
#'


causal_forest_handler <- function(data, covariate_names, share_train = .5, ...) {

  if(!requireNamespace("grf")){
    message("The causal_forest_helper function requires the 'grf' package.")
    return(invisible())
  }

  X <- as.matrix(data %>% select(all_of(covariate_names)))

  train <- complete_rs(nrow(data), prob = share_train) == 1

  cf <-
    grf::causal_forest(X = X[train,],
                  Y = data$Y[train],
                  W = data$Z[train],
                  ...)

  # Prep and return data
  data$pred <- NA
  data$pred[train]  <-
    predict(cf, estimate.variance = FALSE)$predictions
  data$pred[!train] <-
    predict(cf, newdata = X[!train, ], estimate.variance = FALSE)$predictions

  data %>%
    mutate(var_imp = grf::variable_importance(cf) %>% which.max,
           train = train,
           test = !train)
}


#' Best predictor function from causal_forest
#'
#' @param data A data.frame of covariates
#' @param covariates A character vector of covariates to assess
#'
#' @return a data.frame of the best predictors
#'
#' @export
#'
#' @importFrom estimatr lm_robust
best_predictor <- function(data, covariate_names, cuts = 20) {
  data.frame(
    inquiry = "best_predictor",
    estimand = lapply(covariate_names, function(j) {
      lm_robust(tau ~ cut(data[[j]], cuts), data = data)$r.squared
    }) %>%
      unlist %>% which.max
  )
}

# stan_re_estimator <- function(data, model) {
#   J      <- nrow(data)
#   df     <- list(J = J, y = data$estimate, sigma = data$std.error)
#   fit    <- sampling(object = model, data = df, refresh = 0)
#   fit_sm <- summary(fit)$summary
#   data.frame(term = c("mu", "tau", "theta[1]"), estimate = fit_sm[,1][c("mu", "tau", "theta[1]")])
# }
#
# stan_re_estimator_theta <- function(data, model) {
#   J      <- nrow(data)
#   df     <- list(J = J, y = data$estimate, sigma = data$std.error)
#   fit    <- sampling(object = model, data = df, refresh = 0)
#   fit_sm <- summary(fit)$summary
#   data.frame(site = 1:J, estimate = fit_sm[,1][paste0("theta[", 1:J, "]")])
# }

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

