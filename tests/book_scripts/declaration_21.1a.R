print('declaration_21.1a.R'); library(DeclareDesign); library(rdddr); library(tidyverse)


rescale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
likert_cut <- function(x) {
  as.numeric(cut(x, breaks = c(-100, 0.1, 0.3, 0.6, 0.8, 100), labels = 1:5))
}
model <- 
  declare_model(
    N = 800,
    female = rbinom(N, 1, prob = 0.51),
    lgbtq = rbinom(N, 1, prob = 0.05),
    linked_fate = sample(1:5, N, replace = TRUE, 
                         prob = c(0.05, 0.05, 0.15, 0.25, 0.5)),
    age = sample(18:80, N, replace = TRUE),
    religiosity = sample(1:6, N, replace = TRUE),
    income = sample(1:12, N, replace = TRUE),
    college = rbinom(N, 1, prob = 0.5),
    blm_familiarity = sample(1:4, N, replace = TRUE),
    U = runif(N),
    blm_support_latent = rescale(
      U + 0.1 * blm_familiarity + 
        0.45 * linked_fate + 
        0.001 * age + 
        0.25 * lgbtq + 
        0.01 * income + 
        0.1 * college + 
        -0.1 * religiosity),
    # potential_outcomes
    blm_support_Z_general = 
      likert_cut(blm_support_latent),
    blm_support_Z_nationalism = 
      likert_cut(blm_support_latent + 0.01 + 
                   0.01 * linked_fate + 
                   0.01 * blm_familiarity),
    blm_support_Z_feminism = 
      likert_cut(blm_support_latent - 0.02 + 
                   0.07 * female + 
                   0.01 * blm_familiarity),
    blm_support_Z_intersectional = 
      likert_cut(blm_support_latent  - 0.05 + 
                   0.15 * lgbtq + 
                   0.01 * blm_familiarity)
  )
