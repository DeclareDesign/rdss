## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
# -------------
# Meta-analysis
# -------------

## ----file = "scripts_declarations/declaration_18.4.R"---------------------------------------------------------------------------------


## ----eval = TRUE----------------------------------------------------------------------------------------------------------------------
diagnosis_18.4 <- diagnose_design(declaration_18.4)


## -------------------------------------------------------------------------------------------------------------------------------------
diagnosis_18.4 %>% 
  reshape_diagnosis(select = c("Bias", "RMSE", "Coverage")) %>% 
  kable(
    booktabs = TRUE,
    align = "c",
    digits = 2,
    caption = "Bias, RMSE, and coverage from the random effects and fixed effect estimators under the models assumed by the estimators."
  )

