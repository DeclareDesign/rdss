## Submission

rdss 1.0.16 restores `estimator_AS_tidy()`, which had stopped returning
estimates, and corrects package metadata.

## Test environments

* local macOS install (R release)
* ubuntu on GitHub Actions (devel, release, oldrel-1)
* windows on GitHub Actions (release)
* macOS on GitHub Actions (release)

## R CMD check results

0 errors | 0 warnings | 1 note

The note is:

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
