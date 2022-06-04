#' Tidy predictions from rrreg
#'
#' Runs prediction based on rrreg model fit and returns tidy data frame output
#'
#' See https://draft.declaredesign.org/experimental-descriptive.html#list-experiments
#'
#' @param fit a model fit object from rrreg in the rr package
#'
#' @return a data.frame of predicted values
#'
#' @export
rr_predict_tidy <- function(fit) {

  if(!requireNamespace("rr")){
    message("The rr_forced_known function requires the 'rr' package.")
    return(invisible())
  }
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
#' Runs amce estimation function and returns tidy data frame output
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param x an amce fit object from cjoint::amce
#' @param alpha Confidence level
#' @param ... Extra arguments to pass to tidy
#'
#' @return a data.frame of estimates
#'
#' @export
#'
#' @importFrom dplyr `%>%` rename select mutate
#' @importFrom stats qnorm
#'
#' @examples
#'
#' \donttest{
#' library(cjoint)
#'
#' data(immigrationconjoint)
#' data(immigrationdesign)
#'
#' # Run AMCE estimator using all attributes in the design
#' results <- amce(Chosen_Immigrant ~  Gender + Education + `Language Skills` +
#'                   `Country of Origin` + Job + `Job Experience` + `Job Plans` +
#'                   `Reason for Application` + `Prior Entry`, data = immigrationconjoint,
#'                 cluster = TRUE, respondent.id = "CaseID", design = immigrationdesign)
#'
#' # Print summary
#' tidy(results)
#' }
#'
tidy.amce <-
  function(x, alpha = 0.05, ...) {

    if(!requireNamespace("cjoint")){
      message("The tidy function for amce objects requires the 'cjoint' package.")
      return(invisible())
    }

    z_score <- qnorm(1 - ((alpha) / 2))
    summary_fit <- cjoint::summary.amce(x)
    summary_fit$amce %>%
      rename(
        estimate = Estimate,
        std.error = `Std. Err`,
        statistic = `z value`,
        p.value = `Pr(>|z|)`,
        attribute = Attribute,
        level = Level
      ) %>%
      select(-" ") %>%
      mutate(
        conf.low = estimate - z_score * std.error,
        conf.high = estimate + z_score * std.error,
      )
  }


#' Conjoint experiment assignment handler: conducts complete random assignment of all attribute levels
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param data A data.frame
#' @param levels_list List of conjoint levels to assign
#'
#' @return a data.frame with random assignment added
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom randomizr complete_ra
#' @importFrom tibble as_tibble
#'
conjoint_assignment <-
  function(data, levels_list) {
    assignment_df <-
      levels_list %>%
      map( ~ complete_ra(N = nrow(data), conditions = .)) %>%
      as_tibble()
    data[,names(assignment_df)] <- assignment_df
    data
  }

#' Conjoint experiment assignment handler: conducts complete random assignment of all attribute levels
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param data A data.frame
#'
#' @return a data.frame with XXX
#'
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup
#'
conjoint_measurement <-
  function(data) {
    data %>%
      conjoint_utility %>%
      group_by(subject, task) %>%
      mutate(choice = as.numeric(c(U[1] > U[2], U[1] <= U[2]))) %>%
      ungroup()
  }

#' Conjoint experiment inquiries handler
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param data A data.frame
#' @param levels_list List of conjoint levels
#'
#' @return a data.frame of estimand values
#'
#' @importFrom dplyr filter `%>%` mutate bind_rows
#' @importFrom rlang `!!` `:=`
#' @importFrom purrr map_dbl map_chr map pmap_dbl as_vector
#'
#' @export
conjoint_inquiries <-
  function(data, levels_list) {

    # AMCE helper: AMCE for a change of attribute from reference to level

    calculate_amce <-
      function(data, levels_list, attribute, reference, level) {

        # A random draw of the other conjoint levels
        data <- conjoint_assignment(data, levels_list)

        first_profiles <- data %>% filter(profile == 1)
        second_profiles <- data %>% filter(profile == 2)

        # Is second_profile chosen when attribute = reference?
        A <- first_profiles %>%
          bind_rows(mutate(second_profiles,!!attribute := factor(reference))) %>%
          conjoint_measurement() %>%
          filter(profile == 2)

        # Is second_profile chosen when attribute = level?
        B <- first_profiles %>%
          bind_rows(mutate(second_profiles,!!attribute := factor(level))) %>%
          conjoint_measurement()%>%
          filter(profile == 2)

        # Average difference
        mean(B$choice - A$choice)
      }


    repetitions <- levels_list %>% map_dbl(length) - 1

    inquiries_df <-
      tibble(
        attribute = rep(names(levels_list), repetitions),
        reference = rep(map_chr(levels_list, ~ .[1]), repetitions),
        level = map(levels_list, ~ as.character(.[-1])) %>% as_vector()
      )

    inquiries_df %>%
      mutate(
        inquiry = paste0(attribute, level),
        estimand = pmap_dbl(inquiries_df, partial(
          calculate_amce, data = data, levels_list = levels_list
        ))
      )
  }
