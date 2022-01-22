## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------
# Two-arm randomized experiments :: Design diagnosis through simulation
# ---------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
eq_3.4_designer <-
  function(N, m, var_Y0, var_Y1, cov_Y0_Y1, mean_Y0, mean_Y1) {
    
    fixed_sample <-
      MASS::mvrnorm(
        n = N,
        mu = c(mean_Y0, mean_Y1),
        Sigma = matrix(c(var_Y0, cov_Y0_Y1, cov_Y0_Y1, var_Y1), nrow = 2),
        empirical = TRUE # this line makes the means and variances "exact" in the sample data
      ) %>%
      magrittr::set_colnames(c("Y_Z_0", "Y_Z_1"))
    
    declare_model(data = fixed_sample) +
      declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
      declare_assignment(Z = complete_ra(N = N, m = m)) +
      declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
      declare_estimator(Y ~ Z, inquiry = "ATE")
    
  }


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- 
  expand_design(designer = eq_3.4_designer,
                N = 100,
                m = seq(10, 90, 10),
                var_Y0 = 1,
                var_Y1 = 2,
                cov_Y0_Y1 = 0.5,
                mean_Y0 = 1.0,
                mean_Y1 = 1.75)

dx <- diagnose_designs(designs, sims = 100, bootstrap_sims = FALSE)



## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------
# Block-randomized experiments
# ----------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_assignment(
    Z = block_ra(blocks = X, block_prob = c(0.2, 0.5)),
    probs =
      obtain_condition_probabilities(Z, blocks = X, 
                                     block_prob = c(0.2, 0.5)),
    ipw = 1 / probs
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    covariates = ~ X,
    model = lm_lin,
    weights = ipw,
    label = "Lin"
  )


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------
# Block-randomized experiments :: Can blocking ever hurt?
# -------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MI <- declare_model(
  N = 100,
  X = sort(rnorm(N)),
  couple = c(1:(N / 2), (N / 2):1),
  U = rnorm(N, sd = 0.1),
  potential_outcomes(Y ~ Z + X * Z + U)
) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

design_complete <- MI +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z)

design_blocked <- MI +
  declare_assignment(Z = block_ra(blocks = couple)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z)


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_designs(design_complete, design_blocked)


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_designs(design_complete, design_blocked)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# Block-randomized experiments :: Simulation comparing blocking to covariate adjustment
# -------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fixed_pop <-
  fabricate(
    N = 100,
    X = rbinom(N, 1, 0.5),
    U = rnorm(N),
    potential_outcomes(Y ~ 0.2*Z + X + U)
  )

MI <-
  declare_model(data = fixed_pop) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data strategies
complete_assignment <- 
  declare_assignment(Z = complete_ra(N = N)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z))
blocked_assignment <- 
  declare_assignment(Z = block_ra(blocks = X)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z))

# Answer strategies
unadjusted_estimator <- declare_estimator(Y ~ Z, inquiry = "ATE")
adjusted_estimator <- declare_estimator(Y ~ Z + X, model = lm_robust, inquiry = "ATE")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design_1 <- MI + complete_assignment + unadjusted_estimator
design_2 <- MI + blocked_assignment + unadjusted_estimator
design_3 <- MI + complete_assignment + adjusted_estimator
design_4 <- MI + blocked_assignment + adjusted_estimator


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnose_designs(list(design_1, design_2, design_3, design_4))


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Block-randomized experiments :: Can controlling for covariates hurt precision?
# ------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prob = 0.5
control_slope = -1

design <-
  declare_model(N = 100,
                X = runif(N, 0, 1),
                U = rnorm(N, sd = 0.1),
                Y_Z_1 = 1*X + U,
                Y_Z_0 = control_slope*X + U
                ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, prob = prob)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + X, model = lm_robust, inquiry = "ATE", label = "OLS") +
  declare_estimator(Y ~ Z, covariates = ~X, model = lm_lin, inquiry = "ATE", label = "Lin")



## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, 
                    control_slope = seq(-1, 1, 0.5), 
                    prob = seq(0.1, 0.9, 0.1))

simulations <- simulate_designs(designs)


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, 
                    control_slope = seq(-1, 1, 0.5), 
                    prob = seq(0.1, 0.9, 0.1))

simulations <- simulate_designs(designs)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------
# Cluster-randomized experiments
# ------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ICC <- 0.9

design <-
  declare_model(
    cluster =
      add_level(
        N = 10,
        cluster_size = rep(seq(10, 50, 10), 2),
        cluster_shock = 
          scale(cluster_size + rnorm(N, sd = 5)) * sqrt(ICC),
        cluster_tau = rnorm(N, sd = sqrt(ICC))
      ),
    individual =
      add_level(
        N = cluster_size,
        individual_shock = rnorm(N, sd = sqrt(1 - ICC)),
        individual_tau = rnorm(N, sd = sqrt(1 - ICC)),
        Y_Z_0 = cluster_shock + individual_shock,
        Y_Z_1 = Y_Z_0 + cluster_tau + individual_tau
      )
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(
    Z = block_and_cluster_ra(clusters = cluster, blocks = cluster_size)
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z,
                    clusters = cluster,
                    inquiry = "ATE")

designs <- redesign(design, ICC = seq(0.1, 0.9, by = 0.4))


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
diagnoses <- diagnose_designs(designs)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------
# Subgroup designs
# ----------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fixed_pop <-
  fabricate(N = 10000,
            X = rbinom(N, 1, 0.2),
            potential_outcomes(
              Y ~ rbinom(N, 1,
                         prob = 0.7 + 0.1 * Z  - 0.4 * X - 0.2 * Z * X))
            )

total_n <- 1000
n_x1 <- 500
# Note: n_x2 = total_n - n_x1

design <-
  declare_population(data = fixed_pop) +
  declare_inquiry(
    CATE_X1 = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]),
    CATE_X0 = mean(Y_Z_1[X == 0] - Y_Z_0[X == 0]),
    diff_in_CATEs = CATE_X1 - CATE_X0
  ) +
  declare_sampling(
    S = strata_rs(strata = X, strata_n = c(total_n - n_x1, n_x1))
    ) +
  declare_assignment(Z = block_ra(blocks = X)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z + X + Z * X, 
                    term = "Z:X", 
                    inquiry = "diff_in_CATEs")


## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, n_x1 = seq(20, 980, by = 96))
simulations <- simulate_designs(designs)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------
# Factorial experiments
# ---------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CATE_Z1_Z2_0 <- 0.2
CATE_Z2_Z1_0 <- 0.1
interaction <- 0.1
N <- 1000

design <-
  declare_model(
    N = N,
    U = rnorm(N),
    potential_outcomes(Y ~ CATE_Z1_Z2_0 * Z1 +
                         CATE_Z2_Z1_0 * Z2 +
                         interaction * Z1 * Z2 + U,
                       conditions = list(Z1 = c(0, 1),
                                         Z2 = c(0, 1)))) +
  declare_inquiry(
    CATE_Z1_Z2_0 = mean(Y_Z1_1_Z2_0 - Y_Z1_0_Z2_0),
    CATE_Z1_Z2_1 = mean(Y_Z1_1_Z2_1 - Y_Z1_0_Z2_1),
    ATE_Z1 = 0.5 * CATE_Z1_Z2_0 + 0.5 * CATE_Z1_Z2_1,
    
    CATE_Z2_Z1_0 = mean(Y_Z1_0_Z2_1 - Y_Z1_0_Z2_0),
    CATE_Z2_Z1_1 = mean(Y_Z1_1_Z2_1 - Y_Z1_1_Z2_0),
    ATE_Z2 = 0.5 * CATE_Z2_Z1_0 + 0.5 * CATE_Z2_Z1_1,
    
    diff_in_CATEs_Z1 = CATE_Z1_Z2_1 - CATE_Z1_Z2_0,
    #equivalently
    diff_in_CATEs_Z2 = CATE_Z2_Z1_1 - CATE_Z2_Z1_0
  ) + 
  declare_assignment(Z1 = complete_ra(N),
                     Z2 = block_ra(Z1)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z1 + Z2)) +
  declare_estimator(Y ~ Z1, subset = (Z2 == 0), 
                    inquiry = "CATE_Z1_Z2_0", label = 1) +
  declare_estimator(Y ~ Z1, subset = (Z2 == 1), 
                    inquiry = "CATE_Z1_Z2_1", label = 2) +
  declare_estimator(Y ~ Z2, subset = (Z1 == 0), 
                    inquiry = "CATE_Z2_Z1_0", label = 3) +
  declare_estimator(Y ~ Z2, subset = (Z1 == 1),
                    inquiry = "CATE_Z2_Z1_1", label = 4) +
  declare_estimator(Y ~ Z1 + Z2, term = c("Z1", "Z2"), 
                    inquiry = c("ATE_Z1", "ATE_Z2"), label = 5) +
  declare_estimator(Y ~ Z1 + Z2 + Z1*Z2, term = "Z1:Z2", 
                    inquiry = c("diff_in_CATEs_Z1", "diff_in_CATEs_Z2"), 
                    label = 6) 


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, N = seq(500, 3000, 500))
simulations <- simulate_design(designs, sims = 100)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------
# Factorial experiments :: Avoiding misleading inferences
# -------------------------------------------------------

## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(
  design,
  CATE_Z1_Z2_0 = seq(0, 0.5, 0.05),
  CATE_Z2_Z1_0 = 0.2,
  interaction = 0
)

simulations <- simulate_design(designs, sims = 500)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------
# Encouragement designs :: Changes to the answer strategy
# -------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    N = 100,
    type = 
      rep(c("Always-Taker", "Never-Taker", "Complier", "Defier"),
          c(0.2, 0.2, 0.6, 0.0)*N),
    U = rnorm(N),
    # potential outcomes of Y with respect to D
    potential_outcomes(
      Y ~ case_when(
        type == "Always-Taker" ~ -0.25 - 0.50 * D + U,
        type == "Never-Taker" ~ 0.75 - 0.25 * D + U,
        type == "Complier" ~ 0.25 + 0.50 * D + U,
        type == "Defier" ~ -0.25 - 0.50 * D + U
      ),
      conditions = list(D = c(0, 1))
    ),
    # potential outcomes of D with respect to Z
    potential_outcomes(
      D ~ case_when(
        Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1,
        Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
        Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
        Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1
      ),
      conditions = list(Z = c(0, 1))
    )
  ) +
  declare_inquiry(
    ATE = mean(Y_D_1 - Y_D_0),
    CACE = mean(Y_D_1[type == "Complier"] - Y_D_0[type == "Complier"])) +
  declare_assignment(Z = conduct_ra(N = N)) +
  declare_measurement(D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ D | Z,
    model = iv_robust,
    inquiry = c("ATE", "CACE"),
    label = "Two stage least squares"
  ) +
  declare_estimator(
    Y ~ D,
    model = lm_robust,
    inquiry = c("ATE", "CACE"),
    label = "As treated"
  ) +
  declare_estimator(
    Y ~ D,
    model = lm_robust,
    inquiry = c("ATE", "CACE"),
    subset = D == Z,
    label = "Per protocol"
  )



## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- diagnose_design(design, sims = sims, bootstrap_sims = b_sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------
# Placebo-controlled experiments
# ------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
compliance_rate <- 0.2

MI <-
  declare_model(
    N = 400,
    type = sample(x = c("Never-Taker", "Complier"), 
                  size = N,
                  prob = c(1 - compliance_rate, compliance_rate),
                  replace = TRUE),
    U = rnorm(N),
    # potential outcomes of Y with respect to D
    potential_outcomes(
      Y ~ case_when(
        type == "Never-Taker" ~ 0.75 - 0.25 * D + U,
        type == "Complier" ~ 0.25 + 0.50 * D + U
      ),
      conditions = list(D = c(0, 1))
    ),
    # potential outcomes of D with respect to Z
    potential_outcomes(
      D ~ if_else(Z == 1 & type == "Complier", 1, 0),
      conditions = list(Z = c(0, 1))
    )
  ) +
  declare_inquiry(CACE = mean(Y_D_1[type == "Complier"] - Y_D_0[type == "Complier"]))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
encouragement_design <-
  MI +
  declare_assignment(Z = conduct_ra(N = N)) +
  declare_measurement(D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ D | Z,
    model = iv_robust,
    inquiry = "CACE",
    label = "2SLS among all units"
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
placebo_controlled_design <-
  MI +
  declare_sampling(S = complete_rs(N)) +
  declare_assignment(Z = conduct_ra(N = N)) +
  declare_measurement(X = if_else(type == "Complier", 1, 0),
                      D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ Z,
    subset = X == 1,
    model = lm_robust,
    inquiry = "CACE",
    label = "OLS among compliers"
  )


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------
# Stepped-wedge experiments
# -------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    units = add_level(
      N = 100, 
      U_unit = rnorm(N)
    ),
    periods = add_level(
      N = 3,
      time = 1:max(periods),
      U_time = rnorm(N),
      nest = FALSE
    ),
    unit_period = cross_levels(
      by = join(units, periods),
      U = rnorm(N),
      potential_outcomes(
        Y ~ scale(U_unit + U_time + time + U) + effect_size * Z
      )
    )
  ) +
  declare_assignment(
    wave = cluster_ra(clusters = units, conditions = 1:max(periods)),
    Z = if_else(time >= wave, 1, 0)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0), subset = time < max(time)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, fixed_effects = ~ periods + units, 
                    clusters = units, 
                    subset = time < max(time), 
                    inquiry = "ATE", label = "TWFE")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------
# Stepped-wedge experiments :: When to use a stepped wedge experiment
# -------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design_single_period <-
  declare_model(
      N = n_units, 
      U_unit = rnorm(N),
      U = rnorm(N),
      effect_size = effect_size,
      potential_outcomes(Y ~ scale(U_unit + U) + effect_size * Z)
  ) +
  declare_assignment(Z = complete_ra(N, m = n_units / 2)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "TWFE")


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------
# Crossover experiments
# ---------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    units = add_level(
      N = 100, 
      U_unit = rnorm(N, sd = 5)
    ),
    periods = add_level(
      N = 2, 
      time = 1:2, 
      U_time = rnorm(N), 
      nest = FALSE
    ),
    unit_periods = cross_levels(
      by = join(units, periods), 
      U = rnorm(N),
      Y_Z_0_Zlag_0 = U_time + U_unit + U,
      Y_Z_1_Zlag_0 = Y_Z_0_Zlag_0 + 0.2,
      Y_Z_0_Zlag_1 = Y_Z_0_Zlag_0 + 0.2 * carryover
    )
  ) +
  declare_inquiry(
    ATE_untreated_before = mean(Y_Z_1_Zlag_0 - Y_Z_0_Zlag_0)
  ) + 
  declare_assignment(
    Z = block_ra(blocks = units, prob = 0.5),
    Zlag = if_else(time == 2 & Z == 0, 1, 0)
  ) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z + Zlag)) +
  declare_estimator(
    Y ~ Z, 
    cluster = units,
    fixed_effects = ~units,
    model = lm_robust, 
    inquiry = "ATE_untreated_before"
  ) 


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------
# Randomized saturation experiments
# ---------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    group = add_level(N = 50, group_shock = rnorm(N)),
    individual = add_level(
      N = 20,
      individual_shock = rnorm(N),
      potential_outcomes(
        Y ~ 0.2 * Z + 0.1 * (S == "low") + 0.5 * (S == "high") +
          group_shock + individual_shock,
        conditions = list(Z = c(0, 1),
                          S = c("low", "high"))
      )
    )
  ) +
  declare_inquiry(
    CATE_S_Z_0 = mean(Y_Z_0_S_high - Y_Z_0_S_low),
    CATE_Z_S_low = mean(Y_Z_1_S_low - Y_Z_0_S_low)
  ) +
  declare_assignment(
    S = cluster_ra(clusters = group, 
                   conditions = c("low", "high")),
    Z = block_ra(blocks = group, 
                 prob_unit = if_else(S == "low", 0.25, 0.75))
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z + S)) +
  declare_estimator(
    Y ~ S,
    model = difference_in_means,
    subset = Z == 0,
    term = "Shigh",
    clusters = group,
    inquiry = "CATE_S_Z_0",
    label = "Effect of high saturation among untreated"
  ) +
  declare_estimator(
    Y ~ Z,
    model = difference_in_means,
    subset = S == "low",
    blocks = group,
    inquiry = "CATE_Z_S_low",
    label = "Effect of treatment at low saturation"
  )



## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_design(design)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------
# Experiments over networks :: Example
# ------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
adj_matrix <-
  fairfax %>%
  as("Spatial") %>%
  poly2nb(queen = TRUE) %>%
  nb2mat(style = "B", zero.policy = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ra_declaration <- declare_ra(N = 238, prob = 0.1)

permutatation_matrix <- 
  ra_declaration %>%
  obtain_permutation_matrix(maximum_permutations = 10000) %>%
  t()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <-
  declare_model(
    data = select(fairfax, -geometry),
    Y_0_0 = pnorm(scale(SHAPE_LEN), sd = 3),
    Y_1_0 = Y_0_0 + 0.02,
    Y_0_1 = Y_0_0 + 0.01,
    Y_1_1 = Y_0_0 + 0.03
  ) +
  declare_inquiry(
    total_ATE = mean(Y_1_1 - Y_0_0),
    direct_ATE = mean(Y_1_0 - Y_0_0),
    indirect_ATE = mean(Y_0_1 - Y_0_0)
  ) +
  declare_assignment(
    Z = conduct_ra(ra_declaration),
    exposure = get_exposure_AS(make_exposure_map_AS(adj_matrix, Z, hop = 1))
  ) +
  declare_measurement(
    Y = case_when(
      exposure == "dir_ind1" ~ Y_1_1,
      exposure == "isol_dir" ~ Y_1_0,
      exposure == "ind1" ~ Y_0_1,
      exposure == "no" ~ Y_0_0
    )
  ) +
  declare_estimator(handler = estimator_AS, 
                    permutatation_matrix = permutatation_matrix, 
                    adj_matrix = adj_matrix)


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
simulations <- simulate_design(design)

