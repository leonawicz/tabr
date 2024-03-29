% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chord_mapping.R
\name{chord_def}
\alias{chord_def}
\title{Define chords}
\usage{
chord_def(fret, id, optional = NA, tuning = "standard", ...)
}
\arguments{
\item{fret}{integer vector defining fretted chord. See details.}

\item{id}{character, the chord type. See details.}

\item{optional}{\code{NA} when all notes required. Otherwise an integer
vector giving the indices of\code{fret} that are considered optional notes
for the chord.}

\item{tuning}{character, string tuning. See \code{tunings} for predefined
tunings. Custom tunings are specified with a similar \code{value} string.}

\item{...}{additional arguments passed to \code{transpose()}.}
}
\value{
a data frame
}
\description{
Function for creating new chord definition tables.
}
\details{
This function creates a tibble data frame containing information defining
various attributes of chords.
It is used to create the \code{guitarChords} dataset, but can be used to create
other pre-defined chord collections.
The tibble has only one row, providing all information for the defined chord.
The user can decide which arguments to vectorize over when creating a chord
collection. See examples.

This function uses a vector of fret integers (\code{NA} for muted string) to
define a chord, in conjunction with a string \code{tuning} (defaults to standard
tuning, six-string guitar). \code{fret} is from lowest to highest pitch strings,
e.g., strings six through one.

The \code{id} is passed directly to the output. It represents the type of chord
and should conform to accepted \code{tabr} notation. See \code{id} column in
\code{guitarChords} for examples.

Note that the \code{semitones} column gives semitone intervals between chord
notes. These count from zero as the lowest pitch based on the tuning of the
instrument, e.g., zero is E2 with standard guitar tuning. To convert these
semitone intervals to standard semitone values assigned to pitches, use
e.g., \code{pitch_semitones("e2")} (40) if that is the lowest pitch and add
that value to the instrument semitone interval values.
This is the explanation, but doing this is not necessary. You can use
\code{\link[=chord_semitones]{chord_semitones()}} to compute semitones directly on pitches in a
chord.
}
\examples{
frets <- c(NA, 0, 2, 2, 1, 0)
chord_def(frets, "m")
chord_def(frets, "m", 6)

purrr::map_dfr(c(0, 2, 3), ~chord_def(frets + .x, "m"))
}
