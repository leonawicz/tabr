library(testthat)
library(tabr)

test_check("tabr")
cleanup <- list.files(tempdir(), "^(file|foo|lilypond|temp|tmp)", full.names = TRUE)
unlink(cleanup, recursive = TRUE, force = TRUE)
cleanup <- list.files(, "^(file|foo|lilypond|temp|tmp)", full.names = TRUE)
unlink(cleanup, recursive = TRUE, force = TRUE)
