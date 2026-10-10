# Changelog

## rdss 1.0.16

- [`did_multiplegt_tidy()`](https://declaredesign.org/r/rdss/reference/did_multiplegt_tidy.md)
  tidies `did_multiplegt(mode = "dyn")`, returning each effect with its
  standard error and confidence interval. The book’s `mode = "old"` call
  returns `NaN` under DIDmultiplegt 2.1.0, which first differences with
  [`stats::lag()`](https://rdrr.io/r/stats/lag.html) and so never sees a
  treatment switch; the function now warns when that happens.
  `mode = "dyn"` needs the ‘polars’ package, from
  <https://rpolars.r-universe.dev>.
- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)
  computes exposure probabilities from `permutatation_matrix` again, so
  the book’s chapter 18 declaration produces estimates rather than
  nothing. Its argument list returns to the one 1.0.14 shipped; the
  version on the main branch had renamed the argument and moved the
  computation out to the caller, which no published code passed.
- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)
  takes optional precomputed exposure probabilities,
  `obs_prob_exposure`. They do not change across simulations, so
  computing them once and passing them in avoids recomputing them on
  every draw. Without it the function computes them from
  `permutatation_matrix` as before.
- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)
  returns its explanatory message instead of erroring when
  ‘interference’ is absent.
- [`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md)’s
  documented argument now matches its signature.
- Declare the R \>= 4.1.0 dependency the code already has, through its
  use of the native pipe. CRAN has noted this on all 13 flavors.
- `hex_add_alpha(col, 1)` returns a valid eight-digit color; it produced
  nine digits because `floor(1 * 256)` is 256.
- Every help page is revised for a first-time reader: each says what the
  function does and what it returns, names the columns it needs (`Y`,
  `Z`, `tau`, `subject`, `task`, `profile`), and links to related
  functions. The
  [`dd_palette()`](https://declaredesign.org/r/rdss/reference/dd_palette.md)
  page lists the palettes that exist, `fairfax` has 238 rows (not 236),
  and the datasets list their columns.
- Remove a second, unused definition of
  [`tidy_stan()`](https://declaredesign.org/r/rdss/reference/tidy_stan.md).
  The deprecated one, which calls
  [`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html),
  was already the one in effect.
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
