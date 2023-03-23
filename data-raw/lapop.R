
library(tidyverse)
library(haven)

lapop_brazil <- read_stata("data-raw/Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")

lapop_brazil <- lapop_brazil %>%
  transmute(
    ID = uniq_id,
    municipality = municipio,
    female = q1 == 2,
    survey_weights = wt,
    trust_police = b18,
    govt_responsive = eff10,
    ideology = l1,
    govt_pride = b4,
    self_efficacy_political = pra2n,
    trust_military = b12,
    trust_supreme_court = b31,
    support_political_system = b6
  ) %>%
  zap_labels() %>%
  na.omit

usethis::use_data(lapop_brazil)

