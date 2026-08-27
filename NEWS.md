# rdss 1.0.16

* `estimator_AS_tidy()` computes exposure probabilities from `permutatation_matrix`
  again, so the book's chapter 18 declaration produces estimates rather than
  nothing. Its argument list returns to the one 1.0.14 shipped; the version on
  the main branch had renamed the argument and moved the computation out to the
  caller, which no published code passed.
* `estimator_AS_tidy()` returns its explanatory message instead of erroring when
  'interference' is absent.
* `estimator_AS_tidy()`'s documented argument now matches its signature.
* Point `URL` and `BugReports` at the GitHub repository.
* Drop a stray zero-byte `_pkgdown 2.yml`, and stop shipping `README.Rmd`, from
  the source tarball.

# rdss 1.0.14

* fixes issue with intermittent test failure
* deprecates tidy_stan in favor of new broom.mixed::tidy function

# rdss 1.0.12

* address bugs with future package

# rdss 1.0.10

* Switch from prediction to marginaleffects.

# rdss 1.0.8

* Documentation updates for CRAN.

# rdss 1.0.6

* Update to new roxygen and R package documentation standards for CRAN.
* Add ability to obtain declarations with get_rdss_file.

# rdss 1.0.4

* No changes (resubmission after CRAN archiving)

# rdss 1.0.2

* Add lapop_brazil dataset, resampled from the LAPOP 2018 survey in Brazil. Used in the RDSS exercises.

# rdss 1.0.0

* First release to CRAN (renamed from rdddr, previously on CRAN)
