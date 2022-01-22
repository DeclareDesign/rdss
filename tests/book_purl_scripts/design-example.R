## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------
# Declaration in code
# -------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- 
  declare_model(
    villages = add_level(N = 660, U_village = rnorm(N, sd = 0.1)),
    citizens = add_level(
      N = 100,
      U_citizen = rnorm(N),
      potential_outcomes(
        Y ~ pnorm(
          U_citizen + U_village +
            0.10 * (Z == "personal") +
            0.15 * (Z == "social")),
        conditions = list(Z = c("neutral", "personal", "social"))
      )
    )
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
inquiry <- declare_inquiry(
  ATE_personal = mean(Y_Z_personal - Y_Z_neutral),
  ATE_social = mean(Y_Z_social - Y_Z_neutral)
)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
n_villages <- 192
citizens_per_village <- 48

data_strategy <-
  declare_sampling(
    S_village = cluster_rs(clusters = villages, n = n_villages),
    filter = S_village == 1) +
  declare_sampling(
    S_citizen = strata_rs(strata = villages, n = citizens_per_village),
    filter = S_citizen == 1) +
  declare_assignment(
  	Z = cluster_ra(
  	  clusters = villages, 
  	  conditions = c("neutral", "personal", "social"),
  	  prob_each = c(0.250, 0.375, 0.375))) + 
  declare_measurement(
    Y_latent = reveal_outcomes(Y ~ Z),
    Y_observed = rbinom(N, 1, prob = Y_latent)
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
answer_strategy <- 
  declare_estimator(Y_observed ~ Z, term = c("Zpersonal", "Zsocial"), 
                    clusters = villages, 
                    model = lm_robust,
                    inquiry = c("ATE_personal", "ATE_social"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
design <- model + inquiry + data_strategy + answer_strategy


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------
# Diagnosis
# ---------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05),
  cost = mean(10 * n_villages + 1 * n_villages * citizens_per_village)
)


## ----eval = do_diagnosis & !exists("do_bookdown")------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- diagnose_design(design, diagnosands = diagnosands, sims = sims, bootstrap_sims = b_sims)


## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# --------
# Redesign
# --------

## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
designs <- redesign(design, 
                    n_villages = c(192, 500), 
                    citizens_per_village = c(25, 50, 75, 100))

diagnosis <- diagnose_designs(designs, diagnosands = diagnosands)

