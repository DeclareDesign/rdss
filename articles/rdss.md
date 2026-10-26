# Getting started with rdss

rdss is the companion package to [*Research Design in the Social
Sciences: Declaration, Diagnosis, and
Redesign*](https://book.declaredesign.org) by Graeme Blair, Alexander
Coppock, and Macartan Humphreys (Princeton University Press, 2023). The
book declares its designs with
[DeclareDesign](https://declaredesign.org/r/declaredesign/). Most
declarations need nothing beyond DeclareDesign, but some call an
estimator from another package whose output DeclareDesign cannot read
directly, or build a model or inquiry that takes more than a line to
write. rdss holds those pieces, along with the datasets the book
analyzes and the helpers that format its tables and figures.

This article lists which book sections use which rdss functions, says
what each needs installed, and works through one declaration from start
to finish.

## Installing

``` r

install.packages("rdss")
```

Each helper wraps an estimator from another package, and rdss lists
those packages as suggested rather than required, so installing rdss
does not install all of them. Install the ones the sections you are
reading need; the table below names them. Two are not on CRAN:

- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)
  wraps the [interference](https://github.com/szonszein/interference)
  package, which is on GitHub only. The CRAN version of rdss carries a
  stub that explains this. To run the book’s experiment over networks,
  install both packages from GitHub:

  ``` r

  remotes::install_github("szonszein/interference")
  remotes::install_github("DeclareDesign/rdss")
  ```

- [`did_multiplegt_tidy()`](https://declaredesign.org/r/rdss/reference/did_multiplegt_tidy.md)
  with `mode = "dyn"` needs polars, from R-universe:

  ``` r

  install.packages("polars", repos = "https://rpolars.r-universe.dev")
  ```

## Functions by book section

| Book section | rdss functions | Also needs |
|----|----|----|
| [Bayesian formalizations](https://book.declaredesign.org/choosing-an-answer-strategy.html#bayesian-formalizations) | [`tidy_stan()`](https://declaredesign.org/r/rdss/reference/tidy_stan.md) (deprecated: use [`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html)) | rstanarm, broom.mixed |
| [Choosing an answer strategy](https://book.declaredesign.org/choosing-an-answer-strategy.html) | `clingingsmith_etal`, `foos_etal` (data) |  |
| [Multilevel regression and poststratification](https://book.declaredesign.org/observational-descriptive.html#multi-level-regression-and-poststratification) | [`post_stratification_helper()`](https://declaredesign.org/r/rdss/reference/post_stratification_helper.md) | lme4, marginaleffects |
| [Process tracing](https://book.declaredesign.org/observational-causal.html#process-tracing) | [`process_tracing_estimator()`](https://declaredesign.org/r/rdss/reference/process_tracing_estimator.md) | CausalQueries |
| [Difference-in-differences](https://book.declaredesign.org/observational-causal.html#difference-in-differences) | [`lag_by_group()`](https://declaredesign.org/r/rdss/reference/lag_by_group.md), [`did_multiplegt_tidy()`](https://declaredesign.org/r/rdss/reference/did_multiplegt_tidy.md) | DIDmultiplegt, polars |
| [Regression discontinuity designs](https://book.declaredesign.org/observational-causal.html#regression-discontinuity-designs) | [`rdrobust_helper()`](https://declaredesign.org/r/rdss/reference/rdrobust_helper.md), [`tidy.rdrobust()`](https://declaredesign.org/r/rdss/reference/tidy.rdrobust.md) | rdrobust |
| [Conjoint experiments](https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments) | [`conjoint_assignment()`](https://declaredesign.org/r/rdss/reference/conjoint_assignment.md), [`conjoint_measurement()`](https://declaredesign.org/r/rdss/reference/conjoint_measurement.md), [`conjoint_inquiries()`](https://declaredesign.org/r/rdss/reference/conjoint_inquiries.md), [`tidy.amce()`](https://declaredesign.org/r/rdss/reference/tidy.amce.md) | cjoint |
| [Experiments over networks](https://book.declaredesign.org/experimental-causal.html#experiments-over-networks) | [`get_exposure_AS()`](https://declaredesign.org/r/rdss/reference/get_exposure_AS.md), [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md), `fairfax` (data) | interference (GitHub), spdep |
| [Meta-analysis](https://book.declaredesign.org/complex-designs.html#meta-analysis) | [`rma_helper()`](https://declaredesign.org/r/rdss/reference/rma_helper.md), [`rma_mu_tau()`](https://declaredesign.org/r/rdss/reference/rma_mu_tau.md) | metafor |
| [Discovery using causal forests](https://book.declaredesign.org/complex-designs.html#discovery-using-causal-forests) | [`causal_forest_handler()`](https://declaredesign.org/r/rdss/reference/causal_forest_handler.md), [`best_predictor()`](https://declaredesign.org/r/rdss/reference/best_predictor.md) | grf |

The remaining datasets (`bonilla_tillery`, `la_voter_file`, and
`lapop_brazil`) are for the book’s exercises.
[`format_num()`](https://declaredesign.org/r/rdss/reference/format_num.md),
[`add_parens()`](https://declaredesign.org/r/rdss/reference/add_parens.md),
[`make_se_entry()`](https://declaredesign.org/r/rdss/reference/make_se_entry.md),
and
[`make_interval_entry()`](https://declaredesign.org/r/rdss/reference/make_interval_entry.md)
format numbers for tables;
[`theme_dd()`](https://declaredesign.org/r/rdss/reference/theme_dd.md),
[`dd_palette()`](https://declaredesign.org/r/rdss/reference/dd_palette.md),
and
[`hex_add_alpha()`](https://declaredesign.org/r/rdss/reference/hex_add_alpha.md)
give figures the book’s look.

## A worked example: a conjoint experiment

The book’s conjoint declaration (declaration 17.5) shows how the helpers
slot into a design. Subjects each see three tasks, and each task pits
two candidate profiles against each other. Profiles vary in gender,
party, and region, and subjects choose the profile with the higher
utility. The utility function encodes the conjecture under study: women
gain support only in the North and East, and the Right only in the North
and South.

``` r

library(rdss)
library(DeclareDesign)
library(cjoint)
library(dplyr)

levels_list <-
  list(
    gender = c("Man", "Woman"),
    party = c("Left", "Right"),
    region = c("North", "South", "East", "West")
  )

conjoint_utility <- function(data) {
  data |>
    mutate(
      U = 0.25 * (gender == "Woman") * (region %in% c("North", "East")) +
        0.5 * (party == "Right") * (region %in% c("North", "South")) + uij
    )
}
```

Three rdss helpers fill the steps that would otherwise each take a page
of code.
[`conjoint_inquiries()`](https://declaredesign.org/r/rdss/reference/conjoint_inquiries.md)
computes the true average marginal component effect (AMCE) of every
attribute level from the utility function.
[`conjoint_assignment()`](https://declaredesign.org/r/rdss/reference/conjoint_assignment.md)
draws the attributes of every profile at random.
[`conjoint_measurement()`](https://declaredesign.org/r/rdss/reference/conjoint_measurement.md)
applies the utility function and records which profile in each task is
chosen. The estimator is
[`cjoint::amce()`](https://rdrr.io/pkg/cjoint/man/amce.html), which
DeclareDesign can use directly because rdss supplies a
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) method for
its output.

``` r

declaration_17.5 <-
  declare_model(
    subject = add_level(N = 500),
    task = add_level(N = 3, task = 1:N),
    profile = add_level(N = 2, profile = 1:2, uij = rnorm(N, sd = 1))
  ) +
  declare_inquiry(
    handler = conjoint_inquiries,
    levels_list = levels_list,
    utility_fn = conjoint_utility
  ) +
  declare_assignment(handler = conjoint_assignment, levels_list = levels_list) +
  declare_measurement(handler = conjoint_measurement, utility_fn = conjoint_utility) +
  declare_estimator(
    choice ~ gender + party + region,
    respondent.id = "subject",
    .method = amce
  )
```

One draw of the data has a row per profile, with the chosen profile
marked:

``` r

set.seed(343)
draw_data(declaration_17.5) |>
  head()
#> # A tibble: 6 × 9
#>   subject  task profile    uij gender party region      U choice
#>   <chr>   <int>   <int>  <dbl> <fct>  <fct> <fct>   <dbl>  <dbl>
#> 1 001         1       1 -1.40  Woman  Left  North  -1.15       0
#> 2 001         1       2  0.523 Man    Left  North   0.523      1
#> 3 001         2       1  0.142 Man    Right East    0.142      1
#> 4 001         2       2 -0.847 Woman  Left  South  -0.847      0
#> 5 001         3       1 -0.412 Man    Left  South  -0.412      1
#> 6 001         3       2 -1.47  Man    Left  South  -1.47       0
```

The inquiries are the true AMCEs, one per non-reference level:

``` r

draw_estimands(declaration_17.5)
#> # A tibble: 5 × 5
#>   attribute reference level inquiry     estimand
#>   <chr>     <chr>     <chr> <chr>          <dbl>
#> 1 gender    Man       Woman genderWoman   0.036 
#> 2 party     Left      Right partyRight    0.074 
#> 3 region    North     South regionSouth  -0.03  
#> 4 region    North     East  regionEast   -0.0667
#> 5 region    North     West  regionWest   -0.105
```

Diagnosing the design compares the
[`amce()`](https://rdrr.io/pkg/cjoint/man/amce.html) estimates with
those targets over many simulated runs of the study:

``` r

set.seed(343)
diagnosis <- diagnose_design(declaration_17.5, sims = 100, bootstrap_sims = FALSE)
diagnosis
#> Research design diagnosis
#>   100 simulations
#>   estimates matched to inquiries on attribute, level (no estimator named an inquiry)
#> 
#>            Design     Inquiry Estimator N Sims Mean Estimand Mean Estimate
#>  declaration_17.5 genderWoman estimator    100          0.03          0.03
#>  declaration_17.5  partyRight estimator    100          0.07          0.07
#>  declaration_17.5  regionEast estimator    100         -0.07         -0.07
#>  declaration_17.5 regionSouth estimator    100         -0.03         -0.04
#>  declaration_17.5  regionWest estimator    100         -0.10         -0.10
#>   Bias SD Estimate RMSE Power Coverage
#>  -0.00        0.02 0.02  0.43     0.90
#>  -0.00        0.02 0.02  0.97     0.91
#>  -0.00        0.03 0.03  0.75     0.91
#>  -0.00        0.03 0.03  0.32     0.93
#>  -0.00        0.02 0.02  0.99     0.95
```

Bias is near zero for every level. Power is not: the design detects the
larger effects almost every time and the smaller ones in well under half
of runs, which is the kind of finding that should send a researcher back
to `N` before fielding the study.

## The book’s declarations and diagnoses

[`get_rdss_file()`](https://declaredesign.org/r/rdss/reference/get_rdss_file.md)
downloads the book’s saved declarations and diagnosis objects from its
[Dataverse archive](https://doi.org/10.7910/DVN/HYVPO5), so you can
inspect a diagnosis without rerunning a long simulation. Its help page
lists the available names.

``` r

diagnosis_9.1 <- get_rdss_file("diagnosis_9.1")
```
