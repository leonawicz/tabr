
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabr <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

**Author:** [Matthew Leonawicz](https://leonawicz.github.io/blog/)
<a href="https://orcid.org/0000-0001-9452-2771" target="orcid.widget">
<image class="orcid" src="https://members.orcid.org/sites/default/files/vector_iD_icon.svg" height="16"></a>
[![gitter](https://img.shields.io/badge/GITTER-join%20chat-green.svg)](https://gitter.im/leonawicz/tabr)
<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![CRAN
status](http://www.r-pkg.org/badges/version/tabr)](https://cran.r-project.org/package=tabr)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/tabr)](https://cran.r-project.org/package=tabr)
[![Rdoc](http://www.rdocumentation.org/badges/version/tabr)](http://www.rdocumentation.org/packages/tabr)
[![Travis-CI Build
Status](https://travis-ci.org/leonawicz/tabr.svg?branch=master)](https://travis-ci.org/leonawicz/tabr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/tabr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/tabr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/leonawicz/tabr/master.svg)](https://codecov.io/github/leonawicz/tabr?branch=master)

## Overview

Create guitar tablature (“tabs”) from R code with `tabr`. The `tabr`
package provides programmatic music notation and a wrapper around
[LilyPond](http://lilypond.org/) for creating quality guitar
tablature.<img src="tabr_logo_small.png" style="float:right;margin-left:10px;width:400px;">

`tabr` offers functions for describing and organizing musical structures
and wraps around the LilyPond backend. LilyPond is an open source music
engraving program for generating high quality sheet music based on
markup syntax. `tabr` generates files following the LilyPond markup
syntax to be subsequently processed by LilyPond into sheet music.

A standalone LilyPond (.ly) file can be created or the package can make
a system call to LilyPond directly to render the guitar tablature output
(pdf or png). While LilyPond caters to sheet music in general, `tabr` is
focused on leveraging it specifically for creating quality guitar
tablature.

## Functionality and support

The `tabr` package offers the following:

  - Render guitar tablature and sheet music to pdf or png.
  - Write accompanying MIDI files that can respect repeat notation and
    transposition in the sheet music (under reasonable conditions).
  - Support tablature for other string instruments besides guitar such
    as bass or banjo.
  - Support for instruments with different numbers of strings.
  - Support for arbitrary instrument tuning.
  - Offers inclusion (or exclusion) of formal music staves above tab
    staves, such as treble and bass clef staves for complete rhythm and
    timing information.
  - Track-specific setup for features like instrument type, tuning and
    supplemental music staves.
  - Provides common notation such as slide, bend, hammer on, pull off,
    slur, tie, staccato, dotted notes, visible and silent rests.
  - Allows arbitrary tuplet structure.
  - Above-staff text annotation.
  - Percent and volta repeat section notation.
  - Note transposition.
  - Staff transposition.
  - Multiple voices per track and multiple tracks per score.
  - Chord symbols above staff
  - Chord fretboard diagrams and chord chart at top of score.
  - A variety of layout control options covering settings from score
    attributions to font size.
  - Optional alternative input format allowing the user to provide
    string/fret combinations (along with key signature and instrument
    tuning) to map to pitch.

Note that MIDI support and string/fret alternative input format support
are not prioritized in ongoing `tabr` development. These are considered
tangential extra features in `tabr` that fall outside the general scope
and intent of the package.

In additional to the R to LilyPond sheet music pipeline, `tabr` also
offers a large collection of functions to support general music
programming operations.

## Installation

You can install tabr from CRAN with:

``` r
install.packages("tabr")
```

You can install tabr from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("leonawicz/tabr")
```

## Basic example

As a brief example, recreate the tablature shown in the image above
(minus the R logo). Here are the steps.

  - Define a musical phrase with `phrase` or the shorthand alias `p`.
  - Add the phrase to a `track`.
  - Add the track to a `score`.
  - Render the score to pdf with `tab`.

The code is shown below, but first some context.

## Constructing a musical phrase

A phrase here does not require a strict definition. Think of it as the
smallest piece of musical structure you intend to string together. The
first argument to `phrase` is a string describing notes of a specific
pitch (or rests: “r”), separated in time by spaces. For chords, just
remove spaces to indicate simultaneous notes. Integers are appended to
indicate the octave number so that the pitch is unambiguous. For
example, a rest followed by a sequence of notes might be given by `notes
= "r a2 c3 f3 d3 a3 f3"`.

The second argument is a similar string giving note metadata. In this
example there is nothing to add but the time durations. Whole notes
taking up an entire measure of music are given by 1, half notes by 2,
quarter notes 4, eighth notes 8, and so on. To specify a quarter note
rest followed by a sequence of eighth notes, use `info =
"4 8 8 8 8 8 8"` (or shorten to just `info = "4 8*6"`). This basic
example does not require specifying additional note information such as
dotted notes for different fractions of time, staccato notes,
ties/slurs, slides, bends, hammer ons and pull offs, etc. These
specifications are currently available in `tabr` to varying degrees of
development and are covered in the vignette tutorials.

The third argument, `string`, is optional but generally important for
guitar tablature. In similar format, it specifies the strings of the
guitar on which notes are played. Providing this information fixes the
fret-string combinations so that LilyPond does not have to guess what
position on the neck of the guitar to play a specific note. An inability
to specify this in various tablature notation software (or laziness by
the user), is a common cause of inaccurate tabs scouring the internet,
where even when the notes are correct they are written in the tab
suggesting they be played in positions no one would sensibly use. Note
that the `x` shown below is just a placeholder indicating no need to
specify a string for the quarter note rest.

The example below employs a couple shortcuts to further reduce typing.
The first is to use the `*` in-string expansion operator mentioned above
to avoid typing a long series of eighth notes. Second, it drops explicit
reference to octave number three since this central octave is the
default octave in LilyPond. This applies to all but the first note
below.

While explicit string numbers are not needed for this example, they are
provided anyway for full context. Dropping the `string` argument would
further reduce typing.

## Score metadata and accessing LilyPond

Finally, specify some song metadata to reproduce the original staff: the
key of D minor, common time, and the tempo.

If LilyPond is installed on your system (and added to your system path
variable on Windows systems), `tab` should call it successfully. Windows
users are recommended to just add LilyPond’s `bin` directory to the
system path. This will take care of LilyPond as well as its bundled
Python and MIDI support. As an example for Windows users, if the
LilyPond executable is at `C:/Program Files
(x86)/LilyPond/usr/bin/lilypond.exe`, then add `C:/Program Files
(x86)/LilyPond/usr/bin` to the system path.

## R code

``` r
library(tabr)

p("r a2 c f d a f", "4 8*6", "x 5 5 4 4 3 4") %>% track %>% score %>%
  tab("phrase.pdf", key = "dm", time = "4/4", tempo = "4 = 120")
```

    #> #### Engraving score to phrase.pdf ####
    #> GNU LilyPond 2.18.2
    #> Processing `./phrase.ly'
    #> Parsing...
    #> Interpreting music...
    #> Preprocessing graphical objects...
    #> Interpreting music...
    #> MIDI output to `./phrase.mid'...
    #> Finding the ideal number of pages...
    #> Fitting music on 1 page...
    #> Drawing systems...
    #> Layout output to `./phrase.ps'...
    #> Converting to `./phrase.pdf'...
    #> Success: compilation successfully completed

<embed src="phrase.pdf?#zoom=175" width="100%" height="300">

</embed>

<br/>

Note above that `tabr` also exports the pipe `%>%` operator. Even given
the hierarchy of objects involved in the series of steps to move from a
phrase to a rendered pdf, a short example like this does not even
require a single assignment. While music can be quite complex and a full
score will be much longer, `tabr` strives to minimize the work while
still forcing some sense of interpretable, organized structure. For long
and complex music, it can require some effort and practice to ensure
your approach to transcription in your R code is not opaque.

## Additional context

Why LilyPond? LilyPond is an exceptional sheet music engraving program.

  - It produces professional, high quality output.
  - It is open source.
  - It offers a command line access point for a programmatic approach to
    music notation.
  - It is developed and utilized by a large community.
  - Most GUI-based applications are WYSIWYG and force a greater
    limitation on what you can do and what it will look like after you
    do it.

## Limitations

This package is not intended to access all LilyPond functionality from
R. If your transcription needs are that great, just use LilyPond
directly. However, when working in R, `tabr` gives you programmatic
access to LilyPond that is quite valuable.

## New in v 0.3.0: more music programming support

Version 0.3.0 of `tabr` adds many new helper functions. These functions
do not directly impact the `tabr`-LilyPond sheet music pipeline. They
expand music programming with `tabr` more generally and can be used
without LilyPond installed. LilyPond is only required for sheet music
engraving. So even if you do not need to create tabs from R code, the
broader suite of `tabr` functions may be useful to you.

Note: I was hopeful that this version of `tabr` would be able to
leverage a new, native bend engraver in LilyPond, but it appears that on
the LilyPond side this development has not progressed since I last
checked.

## Other resources

<img src="https://raw.githubusercontent.com/r-music/site/master/img/logo.png" style="float:left;margin-right:20px;" width="120">

<div>

<h3 style="padding-top:50px;">

R-Music

</h3>

<h4 style="padding:0px;margin-top:5px;margin-bottom:5px;">

R for music data extraction and analysis

</h4>

See the [R-Music](https://github.com/r-music) organization on GitHub for
more R packages related to music data extraction and analysis.<br/>The
R-Music [blog](https://r-music.rbind.io/) provides package introductions
and examples.

</div>

### Other packages

  - The [tuneR](https://CRAN.R-project.org/package=tuneR) package for
    analysis of music and speech by Uwe Ligges, Sebastian Krey, Olaf
    Mersmann, and Sarah Schnackenberg.
