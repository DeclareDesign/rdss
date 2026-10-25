## Submission

This release changes the maintainer from Graeme Blair <graeme.blair@gmail.com> to Alexander Coppock <acoppock@gmail.com>. Graeme Blair remains an author and will confirm the change by email.

rdss 1.0.16 keeps the package working with DeclareDesign 2.0.0, which is the reason for the release. It also declares `Depends: R (>= 4.1.0)`, which clears the "Missing dependency on R >= 4.1.0" note reported on all 13 check flavors for 1.0.14 (the package code uses the native pipe), and fixes several helper functions used by the book.

As in 1.0.14, `estimator_AS_tidy()` is a stub in the CRAN version: it keeps its signature and returns a message saying how to install the GitHub version. The function wraps the 'interference' package, which is distributed only from GitHub, so the CRAN version neither suggests 'interference' nor carries a `Remotes` field.

## Test environments

* win-builder: R-devel (2026-10-24 r90762 ucrt), 1 NOTE, on the submitted tarball
* local macOS, R 4.6.1, `R CMD check --as-cran`: Status OK
* ubuntu on GitHub Actions (devel, release, oldrel-1): OK
* windows on GitHub Actions (release): OK
* macOS on GitHub Actions (release): OK

The GitHub Actions runs are of the development version, which differs from this submission only in carrying the working `estimator_AS_tidy()` and its test.

## R CMD check results

0 errors | 0 warnings | 1 note

The note, from CRAN incoming feasibility, reports the maintainer change described above and that 'polars' is not in a mainstream repository. 'polars' is in Suggests and is available from https://rpolars.r-universe.dev, which is named in `Additional_repositories`. It is needed only by `did_multiplegt_tidy(mode = "dyn")`, whose example runs only when 'polars' is installed.

## Reverse dependencies

DeclareDesign, by the same authors, suggests rdss. This release is coordinated with DeclareDesign 2.0.0.
