## Test environments
* local Windows 10 install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## This is an update release.

* Previously suggested by CRAN maintainter that functions that create MIDI files via LilyPond should be turned off with `\dontrun{}` because they take too long to execute. These are now turned off.
* Other minor bug fixes.

## Downstream dependencies

I have also run R CMD check on downstream dependencies of rtrek 
(https://github.com/leonawicz/rtrek/blob/master/revdep/checks.rds). 
All packages passed.
