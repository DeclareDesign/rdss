library(DeclareDesign); library(rdddr); library(tidyverse)

## ---- echo = TRUE---------------------------------------------------------------------------------------
# -------------------------------------
# Estimating diagnosands via simulation
# -------------------------------------

## ----results = "hide"-----------------------------------------------------------------------------------
sims <- 500
p.values <- rep(NA, sims)

for(i in 1:sims){
  Z <- rbinom(100, 1, 0.5)
  U <- rnorm(100)
  Y <- 0.2 * Z + U
  p.values[i] <- summary(lm(Y ~ Z))$coefficients[2, 4]
}

power <- mean(p.values <= 0.05)
power


## ----file = "scripts_declarations/declaration_10.1.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosands <- declare_diagnosands(power = mean(p.value <= 0.05))

diagnosis <- 
  diagnose_design(declaration_10.1, 
                  diagnosands = diagnosands)

diagnosis


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Estimating diagnosands via simulation :: Design diagnosis through simulation
# ----------------------------------------------------------------------------

## ----file = "scripts_declarations/designer_17.1.R"------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosands <- 
  declare_diagnosands(
    power = mean(p.value <= 0.05),
    bias = mean(estimate - estimand),
    coverage = mean(estimand <= conf.high & estimand >= conf.low),
    variance = pop.var(estimate)
  )

diagnosis_17.1 <- 
  expand_design(designer = designer_17.1,
                N = 100,
                m = seq(10, 90, 10),
                var_Y0 = 1,
                var_Y1 = 2,
                cov_Y0_Y1 = 0.5,
                mean_Y0 = 1.0,
                mean_Y1 = 1.75) %>% 
  diagnose_designs(diagnosands = diagnosands)


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ----------------------------------------------------------------
# Estimating diagnosands via simulation :: Breaking down diagnosis
# ----------------------------------------------------------------

## ----eval = TRUE----------------------------------------------------------------------------------------
set.seed(343)

simulation_4.1 <- 
  simulate_design(declaration_10.1, sims = 10) %>%
  mutate(significant = p.value <= 0.05)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------
run_design(declaration_10.1)


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_10.3 <-
  diagnose_design(
    declaration_10.1,
    diagnosands = declare_diagnosands(
      bias = mean(estimate - estimand),
      true_se = sd(estimate),
      power = mean(p.value <= 0.05),
      coverage = mean(estimand <= conf.high &
                        estimand >= conf.low)
    )
  )


## ---- echo = TRUE---------------------------------------------------------------------------------------
# ---------------------------------
# Diagnosis under model uncertainty
# ---------------------------------

## ----file = "scripts_declarations/declaration_10.2.R"---------------------------------------------------


## ---- echo = TRUE---------------------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Diagnosis under model uncertainty :: Adjudicating between competing models
# --------------------------------------------------------------------------

## ----file = "scripts_declarations/declaration_10.3.R"---------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------
diagnosis_10.5 <-
  diagnose_design(
    list(declaration_10.3a, declaration_10.3b)
  )


