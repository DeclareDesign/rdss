# rdss 1.0.16

* In the GitHub version, `estimator_AS_tidy()` returns standard errors, p-values, and 95 percent confidence intervals. `interference::estimates()` computes effect variances only when told which condition is control, and rdss had never passed `control_condition`, so the variances were always empty. The Horvitz-Thompson rows use Aronow and Samii's conservative variance, the larger of the Horvitz-Thompson and constant-effects variances, since the plain Horvitz-Thompson variance of a difference can be negative. Intervals for an effect whose exposure condition is rare can undercover; the documentation gives the simulated rates.

* On CRAN, `estimator_AS_tidy()` is a stub that says how to install the GitHub version, as it was in 1.0.14, because the 'interference' package it wraps is not in any repository CRAN can reach. The GitHub version carries the working function. The stub's message now points at `DeclareDesign/rdss` itself; 1.0.14's pointed at a `remotes` branch that no longer exists.

* `post_stratification_helper()` passes `re.form = NA` to `marginaleffects::predictions()` only when the fit is a mixed model. With an `lm_robust()` or other single-level fit, marginaleffects warned that `re.form` is not a known argument on every call.

* `did_multiplegt_tidy()` tidies `did_multiplegt(mode = "dyn")`, returning each effect with its standard error and confidence interval. The book's `mode = "old"` call returns `NaN` under DIDmultiplegt 2.1.0, which first differences with `stats::lag()` and so never sees a treatment switch; the function now warns when that happens. `mode = "dyn"` needs the 'polars' package, from https://rpolars.r-universe.dev.

* `estimator_AS_tidy()` computes exposure probabilities from `permutatation_matrix` again, so the book's chapter 18 declaration produces estimates rather than nothing. Its argument list returns to the one 1.0.14 shipped; the version on the main branch had renamed the argument and moved the computation out to the caller, which no published code passed.

* `estimator_AS_tidy()` takes optional precomputed exposure probabilities, `obs_prob_exposure`. They do not change across simulations, so computing them once and passing them in avoids recomputing them on every draw. Without it the function computes them from `permutatation_matrix` as before.

* `estimator_AS_tidy()` returns its explanatory message instead of erroring when 'interference' is absent.

* `estimator_AS_tidy()`'s documented argument now matches its signature.

* Declare the R >= 4.1.0 dependency the code already has, through its use of the native pipe. CRAN has noted this on all 13 flavors.

* `hex_add_alpha(col, 1)` returns a valid eight-digit color; it produced nine digits because `floor(1 * 256)` is 256.

* Every help page is revised for a first-time reader: each says what the function does and what it returns, names the columns it needs (`Y`, `Z`, `tau`, `subject`, `task`, `profile`), and links to related functions. The `dd_palette()` page lists the palettes that exist, `fairfax` has 238 rows (not 236), and the datasets list their columns.

* Remove a second, unused definition of `tidy_stan()`. The deprecated one, which calls `broom.mixed::tidy()`, was already the one in effect.

* A Get started article on the package website lists the rdss functions each section of the book uses, and works through the conjoint declaration. The datasets cite their sources, and the package has a hex logo.

* Point `URL` and `BugReports` at the GitHub repository.

* Drop a stray zero-byte `_pkgdown 2.yml`, and stop shipping `README.Rmd`, from the source tarball.

# rdss 1.0.14

* Fix an intermittent test failure.
* Deprecate `tidy_stan()` in favor of `broom.mixed::tidy()`.

# rdss 1.0.12

* Address bugs with the future package.

# rdss 1.0.10

* Switch from prediction to marginaleffects.

# rdss 1.0.8

* Documentation updates for CRAN.

# rdss 1.0.6

* Update to new roxygen and R package documentation standards for CRAN.
* Add `get_rdss_file()` to download the book's declarations.

# rdss 1.0.4

* No changes (resubmission after CRAN archiving).

# rdss 1.0.2

* Add the `lapop_brazil` dataset, resampled from the LAPOP AmericasBarometer 2018/19 survey of Brazil. Used in the RDSS exercises.

# rdss 1.0.0

* First release to CRAN (renamed from rdddr, previously on CRAN).
