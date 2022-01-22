## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------
# Conjoint experiments
# --------------------

## ----include = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
calculate_amces <-
  function(candidate_individuals_df) {
    candidate_individuals_df_2 <-
      candidate_individuals_df %>%
      transmute(ID,
                candidate_2 = candidate,
                evaluation_2 = evaluation)
    
    
    average_win_rates <-
      left_join(candidate_individuals_df, candidate_individuals_df_2, by = "ID") %>%
      group_by(f1, f2, f3) %>%
      summarise(win_rate = mean((sign(
        evaluation - evaluation_2
      ) + 1) / 2), .groups = "drop")
    
    
    f1_amces <-
      average_win_rates %>%
      pivot_wider(names_from = f1, values_from = win_rate) %>%
      summarise(`AMCE Woman` = mean(woman - man), .groups = "drop") %>%
      pivot_longer(cols = everything(),
                   names_to = "inquiry",
                   values_to = "estimand")
    f2_amces <-
      average_win_rates %>%
      pivot_wider(names_from = f2, values_from = win_rate) %>%
      summarise(
        `AMCE Old` = mean(old - young),
        `AMCE Middle-aged` = mean(middleaged - young),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = everything(),
                   names_to = "inquiry",
                   values_to = "estimand")
    
    f3_amces <-
      average_win_rates %>%
      pivot_wider(names_from = f3, values_from = win_rate) %>%
      summarise(`AMCE Public` = mean(public - private),
                .groups = "drop") %>%
      pivot_longer(cols = everything(),
                   names_to = "inquiry",
                   values_to = "estimand")
    
    
    bind_rows(f1_amces, f2_amces, f3_amces)
  }


pair_fun <- function(evaluation) {
  if (evaluation[1] > evaluation[2]) {
    ret <- c(1, 0)
  } else if (evaluation[1] < evaluation[2]) {
    ret <- c(0, 1)
  } else if (sample(c(TRUE, FALSE), size = 1)) {
    ret <- c(0, 1)
  } else {
    ret <- c(1, 0)
  }
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
f1 = c("man", "woman")
f1_prob = c(0.5, 0.5)
f2 = c("young", "middleaged", "old")
f2_prob = c(0.25, 0.50, 0.25)
f3 = c("private", "public")
f3_prob = c(0.5, 0.5)


candidates_df <-
  bind_cols(expand_grid(f1, f2, f3),
            expand_grid(f1_prob, f2_prob, f3_prob)) %>%
  mutate(
    candidate = paste(f1, f2, f3, sep = "_"),
    woman = as.numeric(f1 == "woman"),
    middleaged = as.numeric(f2 == "middleaged"),
    old = as.numeric(f2 == "old"),
    public = as.numeric(f3 == "public"),
    prob = f1_prob * f2_prob * f3_prob
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
individuals_df <-
  fabricate(
    N = 1000,
    beta_woman = rnorm(N, mean = 0.1, sd = 1),
    beta_middleaged = rnorm(N, mean = 0.1, sd = 1),
    beta_old = rnorm(N, mean = -0.1, sd = 1),
    beta_public = rnorm(N, mean = 0.1, sd = 1),
    # two way interactions
    beta_woman_middleaged = rnorm(N, mean = -0.05, sd = 0.25),
    beta_woman_old = rnorm(N, mean = -0.05, sd = 0.25),
    beta_woman_public = rnorm(N, mean = 0.05, sd = 0.25),
    beta_public_middleaged = rnorm(N, mean = 0.05, sd = 0.25),
    beta_public_old = rnorm(N, mean = 0.05, sd = 0.25),
    # three-way interactions
    beta_woman_public_middleaged = 0,
    beta_woman_public_old = 0,
    # Idiosyncratic error
    U = rnorm(N, sd = 1)
  )


## ----results = "hide"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
candidate_individuals_df <-
  left_join(candidates_df, individuals_df, by = character()) %>%
  mutate(
    evaluation =
      beta_woman * woman +
      beta_middleaged * middleaged +
      beta_old * old +
      beta_public * public +
      
      beta_woman_middleaged * woman * middleaged +
      beta_woman_old * woman * old +
      beta_woman_public * woman * public +
      beta_public_middleaged * public * middleaged +
      beta_public_old  * public * old +
      
      beta_woman_public_middleaged * woman * public * middleaged +
      beta_woman_public_old * woman * public * old +
      U
  )
inquiries_df <- calculate_amces(candidate_individuals_df)
inquiries_df


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
individuals_wide_df <-
  candidate_individuals_df %>%
  transmute(ID, candidate, evaluation) %>%
  pivot_wider(id_cols = ID,
              names_from = candidate,
              values_from = evaluation)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(
    data = individuals_wide_df,
    pair = add_level(N = 4),
    candidate = add_level(N = 2)
  ) +
  declare_inquiry(
    handler = function(data){inquiries_df}
  ) +
  declare_assignment(
    f1 = complete_ra(N, conditions = f1, prob_each = f1_prob),
    f2 = complete_ra(N, conditions = f2, prob_each = f2_prob),
    f3 = complete_ra(N, conditions = f3, prob_each = f3_prob),
    candidate = paste(f1, f2, f3, sep = "_")
  ) +
  # This function picks out the evaluation of the correct candidate
  declare_measurement(
    handler = function(data) {
      data %>% 
        rowwise %>%
        mutate(evaluation = get(candidate)) %>% 
        ungroup
    }
  ) +
  # This function returns a winner and a loser
  declare_measurement(
    handler = function(data) {
      data %>%
        group_by(pair) %>%
        mutate(Y = pair_fun(evaluation)) %>%
        ungroup
    }
  ) +
  declare_estimator(
    Y ~ f1 + f2 + f3,
    model = lm_robust,
    clusters = ID,
    se_type = "stata",
    term = c("f1woman", "f2middleaged", "f2old", "f3public"),
    inquiry = c("AMCE Woman", "AMCE Middle-aged", "AMCE Old", "AMCE Public")
  )


