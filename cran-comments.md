## Submission

rdss 1.0.16 restores `estimator_AS_tidy()`, which had stopped returning
estimates, and corrects package metadata.

## Test environments

* local macOS install (R release)
* ubuntu on GitHub Actions (devel, release, oldrel-1)
* windows on GitHub Actions (release)
* macOS on GitHub Actions (release)

## R CMD check results

0 errors | 0 warnings | 2 notes

The notes are:

    Suggests or Enhances not in mainstream repositories:
      interference

This is intentional and unchanged in character from earlier releases. A single
function, `estimator_AS_tidy()`, wraps the 'interference' package, which
implements the Aronow and Samii estimator for experiments over networks and is
distributed only from https://github.com/szonszein/interference. The function is
used conditionally: it calls `requireNamespace("interference")` first and, when
the package is absent, returns invisibly after a message naming the install
command. Nothing else in the package depends on it, no example or test requires
it, and the package's own tests skip that file when it is not installed.

The second note is:

    Unknown, possibly misspelled, fields in DESCRIPTION:
      'Remotes'

'Remotes' is present so that the package's own continuous integration, and
anyone installing from GitHub, can resolve the suggested 'interference'
package. Without it the dependency solve fails before any check runs. It is
inert for a CRAN installation, which never reads the field.
