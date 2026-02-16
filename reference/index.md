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

These functions declare research design steps

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
  : Extract mu and tau parameters from rma model fit

## Helpers

- [`best_predictor()`](https://declaredesign.org/r/rdss/reference/best_predictor.md)
  : Best predictor function from causal_forest
- [`conjoint_assignment()`](https://declaredesign.org/r/rdss/reference/conjoint_assignment.md)
  : Conjoint experiment assignment handler: conducts complete random
  assignment of all attribute levels
- [`conjoint_inquiries()`](https://declaredesign.org/r/rdss/reference/conjoint_inquiries.md)
  : Conjoint experiment inquiries handler
- [`conjoint_measurement()`](https://declaredesign.org/r/rdss/reference/conjoint_measurement.md)
  : Conjoint experiment assignment handler: conducts complete random
  assignment of all attribute levels
- [`get_exposure_AS()`](https://declaredesign.org/r/rdss/reference/get_exposure_AS.md)
  : Helper function to obtain the observed exposure for the Aronow and
  Samii estimator
- [`lag_by_group()`](https://declaredesign.org/r/rdss/reference/lag_by_group.md)
  : Generate lags in grouped data

## Tidiers

These functions declare research design steps

- [`tidy(`*`<amce>`*`)`](https://declaredesign.org/r/rdss/reference/tidy.amce.md)
  : Tidy estimates from the amce estimator
- [`tidy(`*`<rdrobust>`*`)`](https://declaredesign.org/r/rdss/reference/tidy.rdrobust.md)
  : Tidy helper function for rdrobust function

## Data

These functions operate on declared designs

- [`bonilla_tillery`](https://declaredesign.org/r/rdss/reference/bonilla_tillery.md)
  : Replication data for Bonilla and Tillery (2020), American Political
  Science Review (obtained from Dataverse 10.7910/DVN/IUZDQI)
- [`clingingsmith_etal`](https://declaredesign.org/r/rdss/reference/clingingsmith_etal.md)
  : Replication data for David Clingingsmith, Asim Ijaz Khwaja, Michael
  Kremer (2020): Estimating the Impact of The Hajj: Religion and
  Tolerance in Islam's Global Gathering. The Quarterly Journal of
  Economics, Volume 124, Issue 3, August 2009, Pages 1133-1170
- [`fairfax`](https://declaredesign.org/r/rdss/reference/fairfax.md) :
  Shapefile of Fairfax County, Virginia, voting precincts
- [`foos_etal`](https://declaredesign.org/r/rdss/reference/foos_etal.md)
  : Replication data for Foos, John, Muller, and Cunningham (2021),
  Journal of Politics (derived from from Dataverse 10.7910/DVN/NDPXND)
- [`la_voter_file`](https://declaredesign.org/r/rdss/reference/la_voter_file.md)
  : Voter file sample for Los Angeles County
- [`lapop_brazil`](https://declaredesign.org/r/rdss/reference/lapop_brazil.md)
  : Data used in student exercises for RDSS based on LAPOP survey of
  Brazil in 2018

## Utilities

These functions operate on declared designs

- [`add_parens()`](https://declaredesign.org/r/rdss/reference/add_parens.md)
  : Add parentheses around standard error estimates
- [`format_num()`](https://declaredesign.org/r/rdss/reference/format_num.md)
  : Round and pad a number to a specific decimal place
- [`make_interval_entry()`](https://declaredesign.org/r/rdss/reference/make_interval_entry.md)
  : Format confidence intervals for nice printing
- [`make_se_entry()`](https://declaredesign.org/r/rdss/reference/make_se_entry.md)
  : Format estimates and standard errors for nice printing
- [`theme_dd()`](https://declaredesign.org/r/rdss/reference/theme_dd.md)
  : ggplot Theme used in the book "Research Design: Declare, Diagnose,
  Redesign" (Blair, Coppock, Humphreys)
- [`dd_palette()`](https://declaredesign.org/r/rdss/reference/dd_palette.md)
  : Access color palette used in the book "Research Design: Declare,
  Diagnose, Redesign" (Blair, Coppock, Humphreys)
- [`hex_add_alpha()`](https://declaredesign.org/r/rdss/reference/hex_add_alpha.md)
  : Add alpha transparency to a color defined in hexadecimal
- [`tidy_stan()`](https://declaredesign.org/r/rdss/reference/tidy_stan.md)
  : Tidy results from a stanreg regresion and exponentiate the estimated
  coefficient
