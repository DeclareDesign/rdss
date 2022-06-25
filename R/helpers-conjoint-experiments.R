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
#' @param utility_fn a function that takes data and returns an additional column called U, which represents the utility of the choice
#'
#' @return a data.frame
#'
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup
#'
conjoint_measurement <-
  function(data, utility_fn) {
    data %>%
      utility_fn %>%
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
#' @param utility_fn a function that takes data and returns an additional column called U, which represents the utility of the choice
#'
#' @return a data.frame of estimand values
#'
#' @importFrom dplyr filter `%>%` mutate bind_rows
#' @importFrom rlang `!!` `:=`
#' @importFrom purrr map_dbl map_chr map pmap_dbl as_vector partial
#'
#' @export
conjoint_inquiries <-
  function(data, levels_list, utility_fn) {

    # AMCE helper: AMCE for a change of attribute from reference to level

    calculate_amce <-
      function(data, levels_list, attribute, reference, level, utility_fn) {

        # A random draw of the other conjoint levels
        data <- conjoint_assignment(data, levels_list)

        first_profiles <- data %>% filter(profile == 1)
        second_profiles <- data %>% filter(profile == 2)

        # Is second_profile chosen when attribute = reference?
        A <- first_profiles %>%
          bind_rows(mutate(second_profiles,!!attribute := factor(reference))) %>%
          conjoint_measurement(utility_fn = utility_fn) %>%
          filter(profile == 2)

        # Is second_profile chosen when attribute = level?
        B <- first_profiles %>%
          bind_rows(mutate(second_profiles,!!attribute := factor(level))) %>%
          conjoint_measurement(utility_fn = utility_fn)%>%
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
          calculate_amce, data = data, levels_list = levels_list, utility_fn = utility_fn
        ))
      )
  }
