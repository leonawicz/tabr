
<!-- README.md is generated from README.Rmd. Please edit that file -->
tabr
====

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/tabr.svg?branch=master)](https://travis-ci.org/leonawicz/tabr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/tabr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/tabr) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/tabr/master.svg)](https://codecov.io/github/leonawicz/tabr?branch=master)

Create guitar tablature ("tabs") from R code with `tabr`. The `tabr` package provides programmatic music notation and a wrapper around [LilyPond](http://lilypond.org/) for creating quality guitar tablature.<img src="tabr_logo_small.png" style="float:right;margin:5px;width:400px;">

`tabr` offers functions for describing and organizing musical structures and a wraps around the LilyPond backend. LilyPond is an open source music engraving program for generating high quality sheet music based on markup syntax. `tabr` generates files following the LilyPond markup syntax to be subsequently processed by LilyPond into sheet music.

A standalone LilyPond (.ly) file can be created or the package can make a system call to LilyPond directly to render the guitar tablature output (pdf or png). While LilyPond caters to sheet music in general, `tabr` is focused on leveraging it specifically for creating quality guitar tablature.

*Note: `tabr` has been tested only on Windows so far. It has not been generalized to Linux and Mac operating systems yet.*

Installation
------------

You can install tabr from GitHub with:

``` r
# install.packages('devtools')
devtools::install_github("leonawicz/tabr")
```

Example
-------

As a brief example, recreate the tablature shown in the logo. Here are the steps.

-   Define a musical phrase with `phrase` or the shorthand alias `p`.
-   Add the phrase to a `track`.
-   Add the track to a `score`.
-   Render the score to pdf with `tab`.

The first argument to `phrase` is a string describing notes of a specific pitch (or rests: "r"), separated in time by spaces. For chords, just remove spaces to indicate simultaneous notes. Integers are appended to indicate the octave number so that the pitch is unique.

The second argument is a similar string giving note metadata. In this example there is nothing to add but the time durations. Whole notes taking up an entire measure of music are given by 1, half notes by 2, quarter notes 4, eighth notes 8, and so on. This simple example does not require additional metadata such as dotted notes, staccato, ties/slurs, slides, bends, hammer ons and pull offs, etc.. These specifications are currently available in `tabr` to varying degrees of development and are covered in the vignette tutorials.

The third argument is optional but generally important for guitar tablature. In similar format, it specifies the strings of the guitar on which notes are played. Providing this information fixes the fret-string combinations so that LilyPond does not have to guess what position on the neck of the guitar to play a specific note. An inability to specify this in various tablature notation software (or laziness by the user), is a common cause of inaccurate tabs scouring the internet, where even when the notes are correct they are written in the tab suggesting they be played in positions no one would sensibly use. The `x` shown is just a placeholder indicating no need to specify a string for the rest.

Finally, we specify some song metadata to reproduce the original: the key of D minor, common time, and the tempo. If LilyPond is installed on your system and added to your system PATH variable, `tab` should call it successfully. Alternatively, it can be added explicitly by calling `tabr_options`. An example of this is commented out below.

``` r
library(tabr)
# tabr_options(lilypond = 'C:/Program Files
# (x86)/LilyPond/usr/bin/lilypond.exe')
p1 <- p("r a2 c3 f3 d3 a3 f3", "4 8 8 8 8 8 8", "x 5 5 4 4 3 4")
track1 <- track(p1)
song <- score(track1)
tab(song, "phrase.pdf", key = "dm", time = "4/4", tempo = "4 = 120")
#> #### Engraving score to phrase.pdf ####
```

<embed src="phrase.pdf?#zoom=175" width="100%" height="300">
</embed>
