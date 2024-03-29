% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chords.R
\name{chord-filter}
\alias{chord-filter}
\alias{chord_root}
\alias{chord_top}
\alias{chord_slice}
\title{Extract notes from chords}
\usage{
chord_root(notes)

chord_top(notes)

chord_slice(notes, index)
}
\arguments{
\item{notes}{character, a noteworthy string.}

\item{index}{integer, the order of a note in a chord by pitch (not scale
degrees).}
}
\value{
a noteworthy string
}
\description{
Filter or slice chords to extract individual notes.
}
\details{
These functions extract notes from chords such as the root note, the highest
pitch, specific position among the notes by pitch, or trim chords to
simplify them.
They operate based only on ordered pitches.

For \code{chord_slice()}, any entry that is empty after slicing is dropped. An
error is thrown is \code{index} is completely out of bounds for all chords.
}
\examples{
x <- "a_2 c#eg# e_gc egc,cc'"
chord_root(x)
chord_top(x)
identical(chord_slice(x, 1), chord_root(x))
chord_slice(x, 2)
chord_slice(x, 4)
chord_slice(x, 3:5)
}
