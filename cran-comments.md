## Test environments
* local Windows 10 install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainter email address update.

## This is an update release.

* Several new functions added, including corresponding documentation, examples, vignettes and unit tests.
* Now skipping one long-running (but successful) unit test that previously caused a CRAN auto-rejection and request for resubmission with fix.

## Downstream dependencies

I have also run R CMD check on downstream dependencies of rtrek 
(https://github.com/leonawicz/rtrek/blob/master/revdep/checks.rds). 
All packages passed.
