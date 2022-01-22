## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------
# Audit experiments
# -----------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
types <- c("Always-Responder","Anti-Latino Discriminator","Never-Responder")
design <-
  declare_model(
    N = 1000,
    type = sample(size = N, 
                  replace = TRUE,
                  x = types,
                  prob = c(0.30, 0.05, 0.65)),
    # Behavioral assumption represented here:
    Y_Z_white = if_else(type == "Never-Responder", 0, 1),
    Y_Z_latino = if_else(type == "Always-Responder", 1, 0)
  ) +
  declare_inquiry(anti_latino_discrimination = mean(type == "Anti-Latino Discriminator")) +
  declare_assignment(Z = complete_ra(N, conditions = c("latino", "white"))) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "anti_latino_discrimination")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------
# Audit experiments :: Intervening to decrease discrimination
# -----------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
N = 5000

design <-
  # This part of the design is about causal inference
  declare_model(
    N = N,
    type_D_0 = sample(
      size = N,
      replace = TRUE,
      x = types,
      prob = c(0.30, 0.05, 0.65)
    ),
    type_tau_i = rbinom(N, 1, 0.5),
    type_D_1 = if_else(
      type_D_0 == "Anti-Latino Discriminator" &
        type_tau_i == 1,
      "Always-Responder",
      type_D_0
    )
  ) +
  declare_inquiry(ATE = mean((type_D_1 == "Anti-Latino Discriminator") -
                               (type_D_0 == "Anti-Latino Discriminator")
  )) +
  declare_assignment(D = complete_ra(N)) +
  declare_measurement(type = reveal_outcomes(type ~ D)) +
  # This part is about descriptive inference in each condition!
  declare_model(
    Y_Z_white = if_else(type == "Never-Responder", 0, 1),
    Y_Z_latino = if_else(type == "Always-Responder", 1, 0)
  ) +
  declare_assignment(
    Z = complete_ra(N, conditions = c("latino", "white"))) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z * D, term = "Zwhite:D", inquiry = "ATE")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------
# List experiments
# ----------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(
    N = 5000,
    control_count = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ Y_star * Z + control_count) 
  ) +
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, model = difference_in_means, 
                    inquiry = "prevalence_rate")


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  mean_CI_width = mean(abs(conf.high - conf.low))
)
diagnosis <- diagnose_design(design, sims = sims, diagnosands = diagnosands)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------
# List experiments :: Assumption violations
# -----------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design_design_effects <- 
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count_control = rbinom(N, size = 3, prob = 0.5),
    control_count_treat = rbinom(N, size = 3, prob = 0.25),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ (Y_star + control_count_treat) * Z + control_count_control * (1 - Z))
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")


## ----eval = do_diagnosis-------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_design_effects <- diagnose_design(design_design_effects, sims = sims)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design_liars <- 
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(
      Y_list ~ 
        if_else(control_count == 3 & Y_star == 1 & Z == 1, 
                3, 
                Y_star * Z + control_count))
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")


## ----eval = do_diagnosis-------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_liars <- diagnose_design(design_liars, sims = sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------
# List experiments :: Choosing design parameters
# ----------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    W = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = proportion_shy)),
    potential_outcomes(Y_list ~ Y_star * Z + control_count)
  ) +
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = n)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z),
                      Y_direct = Y_star - W) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate", label = "list") + 
  declare_estimator(Y_direct ~ 1, inquiry = "prevalence_rate", label = "direct")

designs <- redesign(design, proportion_shy = seq(from = 0, to = 0.3, by = 0.1), n = seq(from = 500, to = 2500, by = 500))


## ----eval = do_diagnosis-------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis_tradeoff <- diagnose_design(designs, sims = sims, bootstrap_sims = b_sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------
# List experiments :: Exercises {-}
# ---------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_rr <-
  declare_model(
    N = 100,
    U = rnorm(N),
    X = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(
      Y_rr ~
        case_when(
          dice == 1 ~ 0L,
          dice %in% 2:5 ~ Y_star,
          dice == 6 ~ 1L
        ),
      conditions = list(dice = 1:6))
  ) + 
  declare_assignment(
    dice = complete_ra(N, prob_each = rep(1/6, 6),
                       conditions = 1:6)) +
  declare_measurement(Y_rr = reveal_outcomes(Y_rr ~ dice)) + 
  declare_estimator(Y_rr ~ 1, handler = label_estimator(rr_forced_known),
                    label = "forced_known", inquiry = "proportion")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_count = rbinom(N, size = J, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ Y_star * Z + control_count) 
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")

designs <- redesign(design, J = 2:5)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis_control_item_count <- diagnose_design(designs, sims = sims)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis_control_item_count %>%
  get_diagnosands %>%
  select(J, bias, rmse) %>%
  kable(booktabs = TRUE, align = "c", digits = 3,
        caption = "Redesign  over number of control items")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 5000,
    U = rnorm(N),
    control_item_1 = draw_binary(0.5, N), 
    control_item_2 = correlate(given = control_item_1, rho = rho, draw_binary, prob = 0.5),
    control_item_3 = draw_binary(0.5, N),
    control_count = control_item_1 + control_item_2 + control_item_3,
    Y_star = rbinom(N, size = 1, prob = 0.3),
    potential_outcomes(Y_list ~ Y_star * Z + control_count)
  ) + 
  declare_inquiry(prevalence_rate = mean(Y_star)) +
  declare_sampling(S = complete_rs(N, n = 500)) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_measurement(Y_list = reveal_outcomes(Y_list ~ Z)) +
  declare_estimator(Y_list ~ Z, inquiry = "prevalence_rate")

designs <- redesign(design, rho = seq(from = 0, to = 1, by = 0.25))


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_control_item_correlation <- diagnose_design(designs, sims = sims, bootstrap_sims = b_sims)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_control_item_correlation %>%
  get_diagnosands %>%
  select(rho, estimator, inquiry, bias, rmse) %>%
  kable(booktabs = TRUE, align = "c", digits = 3)


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



## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------
# Behavioral games
# ----------------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

returned <- function(x1, a_2 = 1 / 3) {
  ((2 * a_2 * x1 - (1 - a_2) * (1 - x1)) / (2 * x1)) * (x1  > (1 - a_2) / (1 + a_2))
}

invested <- function(a_1, a_2) {
  u_a = (1 - a_1) * log(1 - a_1) + a_1 * log(2 * a_1)  # give a1
  u_b = (1 - a_1) * log(2 * a_2) + a_1 * log(2 * (1 - a_2)) # give 1
  ifelse(u_a > u_b, a_1, 1)
}

average_invested <- function(a_1) {
  mean(sapply(seq(0, 1, .01),  invested, a_1 = a_1))
}

average_returned <- function(a_2) {
  mean(sapply(seq(0.01, 1, .01), returned, a_2 = a_2))
}

rho     <- 0.8
n_pairs <- 200

design <-
  declare_model(N = 2 * n_pairs,
                a = runif(N),
                arrival = rank(correlate(given = a, rho = rho, runif))) +
  declare_inquiries(
    mean_invested = mean(sapply(a, average_invested)),
    mean_returned = mean(sapply(a, average_returned)),
    return_from_1 = mean(returned(1, a))
  ) +
  declare_assignment(pair = (arrival - 1) %% n_pairs,
                     role = 1 + (arrival > n_pairs)) +
  declare_step(
    id_cols = pair,
    names_from = role,
    values_from = c(ID, a),
    handler = tidyr::pivot_wider
  ) +
  declare_measurement(invested = invested(a_1, a_2),
                      returned = returned(invested, a_2)) +
  declare_estimator(invested ~ 1,
                    model = lm_robust,
                    inquiry = "mean_invested",
                    label = "mean_invested") +
  declare_estimator(returned ~ 1,
                    model = lm_robust,
                    inquiry = "mean_returned",
                    label = "mean_returned") +
  declare_estimator(
    returned ~ 1,
    model = lm_robust,
    subset = invested == 1,
    inquiry = "return_from_1",
    label = "return_from_1"
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- 
  design %>%
  replace_step(3, declare_assignment(pair = complete_ra(N=N, num_arms = n_pairs))) %>%
  replace_step(4, declare_assignment(role = 1 + block_ra(blocks = pair))) 

