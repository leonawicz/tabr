#' Predefined instrument tunings
#'
#' A data frame containing some predefined instrument tunings commonly used for
#' guitar, bass, mandolin, banjo, ukulele and orchestral instruments.
#'
#' @format A data frame with 2 columns for the tuning ID and corresponding
#' pitches and 32 rows for all predefined tunings.
"tunings"

.map_tuning <- function(x){
  if(x %in% tabr::tunings$id) return(tabr::tunings$value[x == tabr::tunings$id])
  if(length(grep("[h-zA-Z]", x))) stop("Invalid `tuning`.", call. = FALSE)
  gsub(" ", "", .octave_to_tick(x))
}

#' tabr syntax
#'
#' A data frame containing descriptions of syntax used in phrase construction
#' in tabr.
#'
#' @format A data frame with 3 columns for syntax description, operators and
#' examples.
"tabrSyntax"

#' Single note articulations and syntax
#'
#' A data frame containing categorized sets of articulations that can be used
#' in phrase construction.
#'
#' @format A data frame with 3 column and 44 rows.
"articulations"

#' Main musical intervals
#'
#' A data frame containing descriptions of the main intervals, simple and
#' compound.
#'
#' @format A data frame with 5 columns and 26 rows
"mainIntervals"

#' Predefined guitar chords
#'
#' A data frame containing information for many predefined guitar chords.
#'
#' @format A data frame with 12 columns and 3,967 rows
"guitarChords"
