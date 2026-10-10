# Package index

## Package

- [`rdss-package`](https://declaredesign.org/r/rdss/reference/rdss.md)
  [`rdss`](https://declaredesign.org/r/rdss/reference/rdss.md) : rdss
  package

## Replication tools

- [`get_rdss_file()`](https://declaredesign.org/r/rdss/reference/get_rdss_file.md)
  : Download a replication file from the dataverse archive for Research
  Design in the Social Sciences: Declaration, Diagnosis, and Redesign

## Tidy helpers

Wrap estimators from other packages for use in design steps such as
`declare_estimator()`

- [`causal_forest_handler()`](https://declaredesign.org/r/rdss/reference/causal_forest_handler.md)
  : Tidy helper function for causal_forest function

- [`rma_helper()`](https://declaredesign.org/r/rdss/reference/rma_helper.md)
  : Helper function for rma function in metafor package

- [`rdrobust_helper()`](https://declaredesign.org/r/rdss/reference/rdrobust_helper.md)
  :

  Helper function for using rdrobust as a model in `declare_estimator`

- [`post_stratification_helper()`](https://declaredesign.org/r/rdss/reference/post_stratification_helper.md)
  : Post stratification estimator helper

- [`did_multiplegt_tidy()`](https://declaredesign.org/r/rdss/reference/did_multiplegt_tidy.md)
  : Tidy helper function for did_multiplegt

- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)
  : Tidy helper function for estimator_AS function

- [`process_tracing_estimator()`](https://declaredesign.org/r/rdss/reference/process_tracing_estimator.md)
  : Process tracing estimator

- [`rma_mu_tau()`](https://declaredesign.org/r/rdss/reference/rma_mu_tau.md)
  : Extract mu and tau-squared from a random-effects meta-analysis

## Helpers

- [`best_predictor()`](https://declaredesign.org/r/rdss/reference/best_predictor.md)
  : Find the covariate that best predicts treatment effect heterogeneity
- [`conjoint_assignment()`](https://declaredesign.org/r/rdss/reference/conjoint_assignment.md)
  : Conjoint experiment assignment handler: conducts complete random
  assignment of all attribute levels
- [`conjoint_inquiries()`](https://declaredesign.org/r/rdss/reference/conjoint_inquiries.md)
  : Conjoint experiment inquiries handler
- [`conjoint_measurement()`](https://declaredesign.org/r/rdss/reference/conjoint_measurement.md)
  : Conjoint experiment measurement handler: records which profile in
  each task is chosen
- [`get_exposure_AS()`](https://declaredesign.org/r/rdss/reference/get_exposure_AS.md)
  : Helper function to obtain the observed exposure for the Aronow and
  Samii estimator
- [`lag_by_group()`](https://declaredesign.org/r/rdss/reference/lag_by_group.md)
  : Generate lags in grouped data

## Tidiers

Return model fits from other packages as tidy data frames

- [`tidy(`*`<amce>`*`)`](https://declaredesign.org/r/rdss/reference/tidy.amce.md)
  : Tidy estimates from the amce estimator
- [`tidy(`*`<rdrobust>`*`)`](https://declaredesign.org/r/rdss/reference/tidy.rdrobust.md)
  : Tidy helper function for rdrobust function
- [`tidy_stan()`](https://declaredesign.org/r/rdss/reference/tidy_stan.md)
  : Tidy results from a stanreg regression (deprecated)

## Data

Datasets used in the book’s examples and exercises

- [`bonilla_tillery`](https://declaredesign.org/r/rdss/reference/bonilla_tillery.md)
  : Replication data for Bonilla and Tillery (2020), American Political
  Science Review (obtained from Dataverse 10.7910/DVN/IUZDQI)
- [`clingingsmith_etal`](https://declaredesign.org/r/rdss/reference/clingingsmith_etal.md)
  : Replication data for Clingingsmith, Khwaja, and Kremer (2009),
  Quarterly Journal of Economics
- [`fairfax`](https://declaredesign.org/r/rdss/reference/fairfax.md) :
  Shapefile of Fairfax County, Virginia, voting precincts
- [`foos_etal`](https://declaredesign.org/r/rdss/reference/foos_etal.md)
  : Replication data for Foos, John, Muller, and Cunningham (2021),
  Journal of Politics (derived from Dataverse 10.7910/DVN/NDPXND)
- [`la_voter_file`](https://declaredesign.org/r/rdss/reference/la_voter_file.md)
  : Voter file sample for Los Angeles County
- [`lapop_brazil`](https://declaredesign.org/r/rdss/reference/lapop_brazil.md)
  : Teaching data based on the 2018 LAPOP survey of Brazil

## Utilities

Format numbers for tables, and the book’s ggplot theme and palette

- [`add_parens()`](https://declaredesign.org/r/rdss/reference/add_parens.md)
  : Add parentheses around standard error estimates
- [`format_num()`](https://declaredesign.org/r/rdss/reference/format_num.md)
  : Round and pad a number to a specific decimal place
- [`make_interval_entry()`](https://declaredesign.org/r/rdss/reference/make_interval_entry.md)
  : Format confidence intervals for nice printing
- [`make_se_entry()`](https://declaredesign.org/r/rdss/reference/make_se_entry.md)
  : Format estimates and standard errors for nice printing
- [`theme_dd()`](https://declaredesign.org/r/rdss/reference/theme_dd.md)
  : ggplot theme used in the book Research Design in the Social Sciences
- [`dd_palette()`](https://declaredesign.org/r/rdss/reference/dd_palette.md)
  : Color palettes used in the book Research Design in the Social
  Sciences
- [`hex_add_alpha()`](https://declaredesign.org/r/rdss/reference/hex_add_alpha.md)
  : Add alpha transparency to a color defined in hexadecimal
