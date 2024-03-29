% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales.R
\name{scale-deg}
\alias{scale-deg}
\alias{scale_degree}
\alias{scale_note}
\alias{note_in_scale}
\alias{chord_degree}
\alias{is_in_scale}
\title{Scale degrees and mappings}
\usage{
scale_degree(
  notes,
  key = "c",
  scale = "diatonic",
  use_root = TRUE,
  strict_accidentals = TRUE,
  naturalize = FALSE,
  roman = FALSE
)

scale_note(deg, key = "c", scale = "diatonic", collapse = FALSE, ...)

note_in_scale(
  notes,
  key = "c",
  scale = "diatonic",
  use_root = TRUE,
  strict_accidentals = TRUE
)

chord_degree(
  notes,
  key = "c",
  scale = "diatonic",
  strict_accidentals = TRUE,
  naturalize = FALSE,
  roman = FALSE
)

is_in_scale(notes, key = "c", scale = "diatonic", strict_accidentals = TRUE)
}
\arguments{
\item{notes}{character, a string of notes.}

\item{key}{character, key signature (or root note) for scale, depending on
the type of \code{scale}.}

\item{scale}{character, the suffix of a supported \verb{scale_*} function.}

\item{use_root}{logical, use lowest pitch in chord. Otherwise yield an
\code{NA} in output.}

\item{strict_accidentals}{logical, whether representation must match key and
scale. See details.}

\item{naturalize}{logical, whether to naturalize any sharps or flats before
obtaiuning the scale degree.}

\item{roman}{logical, return integer scale degrees as Roman numerals.}

\item{deg}{integer, roman class, or character roman, the scale degree.}

\item{collapse}{logical, collapse result into a single string ready for
phrase construction.}

\item{...}{additional arguments passed to the scale function, e.g.,
\code{sharp = FALSE} for \code{scale_chromatic()}.}
}
\value{
integer, or roman class if \code{roman = TRUE} for \code{scale_degree()};
character for \code{scale_note()}.
}
\description{
These functions assist with mapping between scale degrees, notes and chords.
}
\details{
Obtain the scale degree of a note in a supported scale with \code{scale_degree()}.
This function works on any noteworthy string. It ignores octave numbering.
Rests and any note not explicitly in the scale return \code{NA}. If \code{deg} is
greater than the number of degrees in the scale, it is recycled, e.g., in C
major 8 starts over as C.

By default, flats and sharps checked strictly against the scale. Setting
\code{strict_accidentals = FALSE} will convert any flats or sharps present,
if necessary based on the combination of \code{key} signature and
\code{scale}. The chromatic scale is a special case where strict accidental
is always ignored.

Not any arbitrary combination of valid \code{key} and valid \code{scale} is valid. For
example, \code{key = "am"} and \code{scale = "harmonic"} is valid, but not with
\code{key = "a"}.

\code{note_in_scale()} is a wrapper around \code{scale_degree()}. To check if full
chords are diatonic to the scale, see \code{\link[=is_diatonic]{is_diatonic()}}.

The inverse of \code{scale_degree()} is \code{scale_note()}, for obtaining the note
associated with a scale degree. This could be done simply by calling a
\verb{scale_*} function and indexing its output directly, but this wrapper is
provided to complement \code{scale_degree()}.
Additionally, it accepts the common Roman numeral input for the degree.
This can be with the \code{roman} class or as a character string.
Degrees return \code{NA} if outside the scale degree range.
}
\examples{
scale_degree("r c, e3 g~ g s g# ceg")
note_in_scale("r c, e3 g~ g s g# ceg")

scale_degree("c e g", roman = TRUE)

scale_degree("c c# d_ e", key = "d")
scale_degree("c c# d_ e", key = "d", strict_accidentals = FALSE)

scale_degree("c, e_3 g' f#ac#", use_root = FALSE)
scale_degree("c, e_3 g' f#ac#", naturalize = TRUE) # lowest chord pitch: c#

scale_degree("c# d_ e_' e4 f f# g", key = "c#", scale = "chromatic")

scale_note(1:3, key = "am")
scale_note(c(1, 3, 8), "d", collapse = TRUE)
all(sapply(list(4, "IV", as.roman(4)), scale_note) == "f")

x <- "d dfa df#a f#ac#"
chord_degree(x, "d")
is_in_scale(x, "d")
}
\seealso{
\code{\link[=scale-helpers]{scale-helpers()}}, \code{\link[=is_diatonic]{is_diatonic()}}
}
