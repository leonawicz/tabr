## Test environments

* Local Windows 10 install: R 4.3.1
* Win Builder: R-devel, R-release
* Fedora: R-devel
* Ubuntu 20.04.1: R-release

## Update release

* Added required package alias per CRAN request.
* New R package version build against new version of Lilypond software: version `2.24.2`. This version will likely need to be updated on the CRAN machine that Lilypond was installed on in the past. Requirement is listed in `DESCRIPTION` `SystemRequirements`.
* Other general package maintenance, bug fixes, documentation updates.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

All checks pass. (https://github.com/leonawicz/tabr/blob/master/revdep/)
