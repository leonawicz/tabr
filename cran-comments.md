## Test environments

* Local Windows 11 install: R 4.5.0
* Win Builder: R-devel, R-release
* Ubuntu latest: R-devel, R-release, R-oldrel
* MacOS latest: R-release

## Re-released of archived package

I made a sincere attempt to fix the package (v0.5.2) before it would be archived. It was republished by CRAN, but the issue which was throwing one NOTE on two specific Linux flavors persisted after the updated package was released to CRAN.

Unfortuantely, I am unable to detect this issue on any other system prior to resubmission, including Win Builder, GitHub, or locally.

This resubmission makes another attempt to fix this opaque temporary cruft file issue ("Check: for non-standard things in the check directory... Found the following files/directories...") that only occurs on a two Linux flavors on CRAN post-release that I cannot detect in any other testing environment.

This issue does not surface until after release. I cannot detect where it occurs in CRAN's Linux Fedora "check directory". I have tried again to prevent this temporary file creation, but it is somewhat guesswork. Can someone please look at this resubmission and tell me where the issue arises specifically, if I still have not managed to remove it with this resubmission?

CRAN comments included from previous update release attempt (v0.5.2):

* Fixed issues requested by CRAN.
* Made best attempt to prevent "Found the following files/directories" NOTE that occurs on two Linux flavors during CRAN submission only. I cannot reproduce this elsewhere or reconfirm without CRAN re-submission. Please advise if still an issue.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

All checks pass. (https://github.com/leonawicz/tabr/blob/master/revdep/)
