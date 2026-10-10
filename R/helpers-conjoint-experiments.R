#' Tidy estimates from the amce estimator
#'
#' A [generics::tidy()] method for fits from [cjoint::amce()]: one row per attribute level, with normal-approximation confidence intervals.
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param x A fit from [cjoint::amce()].
#' @param alpha The significance level for the confidence intervals. Defaults to 0.05, which gives 95 percent intervals.
#' @param ... Not used.
#'
#' @return A data frame with columns `attribute`, `level`, `estimate`, `std.error`, `statistic`, `p.value`, `conf.low`, and `conf.high`.
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
#' For each attribute, assigns every row one of that attribute's levels by complete random assignment, so the levels appear in as equal numbers as possible.
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param data A data frame with one row per profile.
#' @param levels_list A named list with one element per attribute, each a vector of that attribute's levels, e.g. `list(gender = c("woman", "man"), age = c("young", "old"))`.
#'
#' @return `data` with one column per attribute, named as in `levels_list`.
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

#' Conjoint experiment measurement handler: records which profile in each task is chosen
#'
#' Applies `utility_fn` and, within each subject and task, compares the two profiles and sets `choice` to 1 for the one with the higher utility `U` (the second profile on a tie).
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param data A data frame with one row per profile and columns `subject`, `task`, and `profile`. Each task must hold exactly two profiles.
#' @param utility_fn A function that takes `data` and returns it with a column `U` added, the utility the subject gets from each profile.
#'
#' @return `data` with `U` and `choice` added.
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
#' Computes the true average marginal component effect (AMCE) of every attribute level relative to that attribute's first level. Each AMCE is the average change in the probability that the second profile in a task is chosen when the attribute moves from its first level to the given level, with the other attributes drawn at random by [conjoint_assignment()].
#'
#' See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
#'
#' @param data A data frame with one row per profile and columns `subject`, `task`, and `profile` (1 or 2).
#' @param levels_list A named list of attribute levels, as in [conjoint_assignment()]. The first level of each attribute is the reference.
#' @param utility_fn A function that adds the utility column `U`, as in [conjoint_measurement()].
#'
#' @return A data frame with one row per non-reference level: `attribute`, `reference`, `level`, `inquiry`, and `estimand`.
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
