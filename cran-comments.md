## Update release

This is an update release submission. In this version I have:

* Added `lintr` package to DESCRIPTION Suggests field per CRAN maintainer request regarding undeclared packages used in `testhat` unit tests.
* Added other minor function and and documentation updates since last submission.

## Test environments
* local Windows 10 install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of tabr 
(https://github.com/leonawicz/tabr/blob/master/revdep/checks.rds). 
All packages passed.
