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
#' @importFrom cjoint summary.amce
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
#' @param data
#' @param levels_list
#'
#' @return
#' @export
#' @importFrom purrr map
#' @importFrom randomizr complete_ra
#' @importFrom tibble as_tibble
#'
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
#' @param data
#'
#' @return
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

#' Conjoint experiment Inquiries handler
#'
#' @param data
#' @param levels_list
#'
#' @importFrom dplyr filter `%>%` mutate bind_rows
#' @importFrom rlang `!!` `:=`
#' @importFrom purrr map_dbl map_chr map pmap_dbl as_vector
#' @return
#'
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
