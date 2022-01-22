## ---- echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------
# Mixed methods
# -------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(CausalQueries)
causal_model <- make_model("X -> M -> Y; M <-> Y") %>%
  set_priors(node = "X", alpha = c(100, 100)) %>%
  set_restrictions(labels = list(M = "10",  Y = "10"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Quantitative data
data_handler = function(data, n = n) 
    CausalQueries::make_data(causal_model, 
                             parameters = data$parameters, 
                             n = n)

# Case selection data targets within X, Y data combinations
strategy <- c("00" = 10, "01" = 0, "10" = 0, "11" = 10)
strategy_names <- names(strategy)

# strat_n flexible to take account of the possibility of no data in some strata
strata_n <- function(strategy, strata) 
  sapply(1:4, function(i) min(strategy[i], sum(strata == strategy_names[i])))[strategy_names %in% strata]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimation_handler = function(data) 
		causal_model %>% update_model(data = data) %>%
    query_model(query = "Y[X=1] - Y[X=0]", 
                using = "posteriors", 
                given = c(TRUE, "X==1 & Y==1")) %>%
  rename(estimate = mean) %>%
  select(estimate, sd) %>% 
  mutate(Inquiry = c("ATE", "POC"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
n <- 50

design <-

  declare_model(data.frame(parameters = CausalQueries::get_parameters(causal_model, param_type = "prior_draw"))) +
  
  declare_inquiry(ATE = CausalQueries::query_model(causal_model, "Y[X=1] - Y[X=0]", 
  parameters = parameters, using = "parameters")$mean) +   
  
  declare_inquiry(POC = CausalQueries::query_model(causal_model, "Y[X=1] - Y[X=0]", given = "X==1 & Y==1",  
  parameters = parameters, using = "parameters")$mean) +
  
  declare_measurement(handler = data_handler, n = n) +
  
  declare_measurement(
    strata = paste0(X,Y),
    M_observed = strata_rs(strata = paste0(X,Y), strata_n = strata_n(strategy, strata)),
    M = ifelse(M_observed==1, M, NA)) +
  
  declare_estimator(handler = label_estimator(estimation_handler), inquiry = c("ATE", "POC")) 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mixed_diagnosands <- 
  declare_diagnosands(mean_estimate = mean(estimate),
                      sd_estimate = sd(estimate),
                      bias = mean(estimate - estimand),
                      posterior_variance = mean(sd^2))



## ----eval = TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagnosis <- design %>%
  redesign(N = c(50, 100, 150), 
           strategy = list(c(0,0,0,0), c(10,0,0,10), c(0,0,0,20))) %>% 
  diagnose_design(sims = 2, diagnosands = mixed_diagnosands)


