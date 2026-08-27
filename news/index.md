# Changelog

## rdss 1.0.16

- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)
  computes exposure probabilities from `permutatation_matrix` again, so
  the book’s chapter 18 declaration produces estimates rather than
  nothing. Its argument list returns to the one 1.0.14 shipped; the
  version on the main branch had renamed the argument and moved the
  computation out to the caller, which no published code passed.
- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)
  returns its explanatory message instead of erroring when
  ‘interference’ is absent.
- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)’s
  documented argument now matches its signature.
- Declare the R \>= 4.1.0 dependency the code already has, through its
  use of the native pipe. CRAN has noted this on all 13 flavors.
- Point `URL` and `BugReports` at the GitHub repository.
- Drop a stray zero-byte `_pkgdown 2.yml`, and stop shipping
  `README.Rmd`, from the source tarball.

## rdss 1.0.14

CRAN release: 2025-01-09

- fixes issue with intermittent test failure
- deprecates tidy_stan in favor of new broom.mixed::tidy function

## rdss 1.0.12

CRAN release: 2024-10-10

- address bugs with future package

## rdss 1.0.10

CRAN release: 2024-03-30

- Switch from prediction to marginaleffects.

## rdss 1.0.8

CRAN release: 2024-03-02

- Documentation updates for CRAN.

## rdss 1.0.6

CRAN release: 2024-02-20

- Update to new roxygen and R package documentation standards for CRAN.
- Add ability to obtain declarations with get_rdss_file.

## rdss 1.0.4

CRAN release: 2023-05-02

- No changes (resubmission after CRAN archiving)

## rdss 1.0.2

CRAN release: 2023-03-27

- Add lapop_brazil dataset, resampled from the LAPOP 2018 survey in
  Brazil. Used in the RDSS exercises.

## rdss 1.0.0

CRAN release: 2023-01-17

- First release to CRAN (renamed from rdddr, previously on CRAN)
