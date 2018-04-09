## Resubmission

This is a resubmission. In this version I have:

* added single quotes around software names ('LilyPond') in the DESCRIPTION.
* ensured all unit tests of the four functions that write files (`lilypond`, `tab`, `midily`, `miditab`) write to `tempdir()` and not to the user's home directory.
* package examples are not run if they depend on 'LilyPond' or write files.

Context: help doc examples of these functions are not run because the 'LilyPond' software is not pre-installed and available in all build environments (e.g., CRAN).
However, examples do an insufficient job of testing these file-writing functions anyway. That is why there is a robust unit test suite to cover these functions.
These specific file-writing tests that rely on 'LilyPond' are still skipped on CRAN, but they are run and pass in other build environments and on different operating systems prior to CRAN submission.

## Test environments
* local Windows 10 install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of tabr 
(https://github.com/leonawicz/tabr/blob/master/revdep/checks.rds). 
All packages passed.
