## Resubmission

This is a resubmission. In this version I have:

* Unwrapped `lilypond` example from `dontrun` tag.
* Switched to `tempdir()` location for examples that write files.
* Added LilyPond to SystemRequirements field in DESCRIPTION.
* PLEASE NOTE: Attempted to wrap these examples in if-statements to only run if LilyPond is found on the system, per CRAN review request. However, I do not know specifically how to check for this on Win-Builder or Windows/Linux/Mac OS CRAN machines in general.

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
