library(DeclareDesign); library(rdddr); library(tidyverse)


answer_strategy <-
  declare_estimator(
    blm_support ~ Z,
    term = c("Znationalism", "Zfeminism", "Zintersectional"),
    inquiry = 
      c("ATE_nationalism", "ATE_feminism", "ATE_intersectional"),
    label = "OLS") +
  declare_estimator(
    blm_support ~ Z + age + female + as.factor(linked_fate) + lgbtq,
    term = c("Znationalism", "Zfeminism", "Zintersectional"),
    inquiry = 
      c("ATE_nationalism", "ATE_feminism", "ATE_intersectional"),
    label = "OLS with controls") +
  declare_estimator(
    blm_support ~ Z*blm_familiarity,
    term = c("Znationalism:blm_familiarity", 
             "Zfeminism:blm_familiarity", 
             "Zintersectional:blm_familiarity"),
    inquiry = c("DID_nationalism_familiarity", 
                "DID_feminism_familiarity", 
                "DID_intersectional_familiarity"),
    label = "DID_familiarity") +
  declare_estimator(
    blm_support ~ Z * linked_fate,
    term = "Zfeminism:linked_fate",
    inquiry = "DID_nationalism_linked_fate",
    label = "DID_nationalism_linked_fate") +
  declare_estimator(
    blm_support ~ Z * female,
    term = "Zfeminism:female",
    inquiry = "DID_feminism_gender",
    label = "DID_feminism_gender") +
  declare_estimator(
    blm_support ~ Z * lgbtq,
    term = "Zintersectional:lgbtq",
    inquiry = "DID_intersectional_lgbtq",
    label = "DID_intersectional_lgbtq")
