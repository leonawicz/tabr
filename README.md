
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabr <img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right">

**Author:** [Matthew Leonawicz](https://leonawicz.github.io/blog/)
<a href="https://orcid.org/0000-0001-9452-2771" target="orcid.widget">
<image class="orcid" src="https://members.orcid.org/sites/default/files/vector_iD_icon.svg" height="16"></a>
<br/> **License:** [MIT](https://opensource.org/licenses/MIT)<br/>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build
Status](https://travis-ci.org/leonawicz/tabr.svg?branch=master)](https://travis-ci.org/leonawicz/tabr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/tabr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/tabr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/leonawicz/tabr/master.svg)](https://codecov.io/github/leonawicz/tabr?branch=master)

[![CRAN
status](http://www.r-pkg.org/badges/version/tabr)](https://cran.r-project.org/package=tabr)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/tabr)](https://cran.r-project.org/package=tabr)
[![Github
Stars](https://img.shields.io/github/stars/leonawicz/tabr.svg?style=social&label=Github)](https://github.com/leonawicz/tabr)

[![Donate](https://img.shields.io/badge/Donate-Buy%20me%20a%20coffee-yellowgreen.svg)](https://ko-fi.com/leonawicz)

## Overview <img src="https://github.com/leonawicz/tabr/blob/master/data-raw/tabr_logo.png?raw=true" width=320 style="float:right;margin-left:10px;width:320px;">

The `tabr` package provides a music notation syntax and a collection of
music programming functions for generating, manipulating, organizing and
analyzing musical information in R.

The music notation framework facilitates creating and analyzing music
data in notation form; i.e, more from the perspective and in the
language of a musician than, say, an audio engineer.

<hr>

*If you enjoy my R community contributions, consider* ***[buying me a
coffee in Ko-fi](https://ko-fi.com/leonawicz)*** *so I can keep
developing and maintaining this and other packages :)*

<hr>

### Music data structures

Music data can be viewed, manipulated and analyzed while in different
forms of representation based around different data structures: strings
and data frames. Each representation offers advantages over the other
for different use cases.

Music syntax can be entered directly and represented in character
strings to minimize the formatting overhead of data entry by using
simple data structures, for example when wanting to quickly enter and
transcribe short pieces of music syntax in R into sheet music or
tablature files. You can also enter sound and time together for the
`music` class, and no need to repeat consecutive durations until a
change.

``` r
x <- "a, c e g# a ac'e' ac'e'~ ac'e' a c' e' a'"
x <- as_noteworthy(x)
x
#> <Noteworthy string>
#>   Format: space-delimited time
#>   Values: a, c e g# a <ac'e'> <ac'e'~> <ac'e'> a c' e' a'

summary(x)
#> <Noteworthy string>
#>   Timesteps: 12 (9 notes, 3 chords)
#>   Octaves: tick
#>   Accidentals: sharp
#>   Format: space-delimited time
#>   Values: a, c e g# a <ac'e'> <ac'e'~> <ac'e'> a c' e' a'

y <- "a,8 c et8 g# a ac'e'4. ac'e'~8 ac'e'4 at4 c' e' a'1"
y <- as_music(y)
summary(y)
#> <Music string>
#>   Timesteps: 12 (9 notes, 3 chords)
#>   Octaves: tick
#>   Accidentals: sharp
#>   Key signature: c
#>   Time signature: 4/4
#>   Tempo: 2 = 60
#>   Lyrics: NA
#>   Format: space-delimited time
#>   Values: a,8 c8 et8 g#t8 at8 <ac'e'>4. <ac'e'~>8 <ac'e'>4 at4 c't4 e't4 a'1

music_split(y)
#> $notes
#> <Noteworthy string>
#>   Format: space-delimited time
#>   Values: a, c e g# a <ac'e'> <ac'e'~> <ac'e'> a c' e' a'
#> 
#> $info
#> <Note info string>
#>   Format: space-delimited time
#>   Values: 8 8 t8 t8 t8 4. 8 4 t4 t4 t4 1
#> 
#> $lyrics
#> [1] NA
#> 
#> $key
#> [1] "c"
#> 
#> $time
#> [1] "4/4"
#> 
#> $tempo
#> [1] "2 = 60"
```

Functions exist for directly performing various mathematical, logical
and organizational operations and musical transformations on strings
like the one above by checking their music syntax validity and adding
custom classes and methods to these strings (more on this below). `tabr`
offers special object classes that facilitate working with music data
and notation in ways that are natural to R, robust, tidy, and lend
themselves well to transcription as well as analysis.

Of course, none of this will work on character strings that are not
“noteworthy” or “musical”, for example. Invalid, unworthy syntax is
rejected early with an error, preventing corrupted music syntax from
causing unexpected issues later on.

The same music data can also be organized in tidy data frames, allowing
for a more familiar and powerful approach to the analysis of large
amounts of structured music data.

``` r
x <- "a,8 c e r r c a, g#, a ac'e'"
as_music(x) %>% as_music_df()
#> # A tibble: 10 x 12
#>    duration pitch note  semitone octave  freq pitch_int scale_int slur 
#>    <chr>    <chr> <chr>    <int>  <int> <dbl>     <int> <chr>     <chr>
#>  1 8        a,    a           57      2  110.        NA <NA>      <NA> 
#>  2 8        c     c           48      3  131.         3 m3        <NA> 
#>  3 8        e     e           52      3  165.         4 M3        <NA> 
#>  4 8        r     r           NA     NA   NA         NA <NA>      <NA> 
#>  5 8        r     r           NA     NA   NA         NA <NA>      <NA> 
#>  6 8        c     c           48      3  131.        -4 M3        <NA> 
#>  7 8        a,    a           57      2  110.        -3 m3        <NA> 
#>  8 8        g#,   g#          56      2  104.        -1 m2        <NA> 
#>  9 8        a     a           57      3  220.        13 m9        <NA> 
#> 10 8        ac'e' ace         57      3  220.         0 P1        <NA> 
#> # ... with 3 more variables: slide <lgl>, dotted <int>, annotation <chr>
```

Several functions are available for mapping seamlessly between and
manipulating these data structures and their representations of musical
information.

### Transcription

Music programming in the notation syntax provided by `tabr` can be used
for a variety of purposes, but it also integrates cohesively with the
package’s transcription functions. The package also provides API wrapper
functions for transcribing music notation in R into guitar tablature
(“tabs”) and basic sheet music using [LilyPond](http://lilypond.org/).

LilyPond is an open source music engraving program for generating high
quality sheet music based on markup syntax. `tabr` generates LilyPond
files from R code and can pass them to LilyPond to be rendered into
sheet music pdf files. While LilyPond caters to sheet music in general
and `tabr` can be used to create basic sheet music, the transcription
functions focus on leveraging LilyPond specifically for creating quality
guitar tablature. You do not need to use it for guitar tablature, but
for vocal or other instrument tracks, you can change settings, such as
suppressing a tab staff from your sheet music.

While LilyPond is listed as a system requirement for `tabr`, you can use
the package for music analysis without installing LilyPond if you do not
intend to render tabs. You can even use the `lilypond` function to write
LilyPond files to disk without the software installed, since this is
only a case of R writing plain text files in the proper format. The only
functions in the package that require a LilyPond installation are `tab`,
`midily`, `miditab` and any `render_*` functions.

### Use case considerations

`tabr` offers a useful but limited LilyPond API and is not intended to
access all LilyPond functionality from R, nor is transcription via the
API the entire scope of `tabr`. If you are only creating sheet music on
a case by case basis, write your own LilyPond files manually. There is
no need to use `tabr` or limit yourself to its existing LilyPond API or
its guitar tablature focus.

However, if you are generating music notation programmatically, `tabr`
provides the ability to do so in R and offers the added benefit of
converting what you write in R code to the LilyPond file format to be
rendered as printable sheet music.

With ongoing development, the music programming side of `tabr` will
likely continue to grow much more than the transcription functionality.

### Why LilyPond for transcription?

LilyPond is an exceptional sheet music engraving program.

  - It produces professional, high quality output.
  - It is open source.
  - It offers a command line access point for a programmatic approach to
    music notation.
  - It is developed and utilized by a large community.
  - Most GUI-based applications are WYSIWYG and force a greater
    limitation on what you can do and what it will look like after you
    do it. It is only for the better that `tabr` is the bottleneck in
    transcription limitations rather than the music engraving software
    it wraps around.

### Transcription functionality and support

The `tabr` package offers the following for transcription:

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
  - If music staff is included, the tab staff can be suppressed, e.g.,
    for vocal tracks.
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

### MIDI support

The package offers nominal MIDI file output support in conjunction with
rendering sheet music. MIDI file writing is still handled by LilyPond,
which means it must be based on a valid LilyPond file output created by
`tabr`.

You can read MIDI files into R. This support relies on the `tuneR`
package to read MIDI files and attempts to structure the MIDI data to
integrate as best as possible with the data structures and functionality
found throughout `tabr`.

An existing MIDI file can also be passed through directly to LilyPond to
attempt to create sheet music from the MIDI file if possible, using one
of LilyPond’s command line utilities for MIDI to LilyPond conversion
followed by rendering the generated LilyPond file to sheet music.

## Installation

### Upcoming v0.4.0 release

Music data analysis has been expanding significantly since version
0.3.0, which brought many new functions to `tabr`. Version 0.3.5 brought
even more as well as an alternative input format for guitar tab
transcription.

The upcoming version 0.4.0 is a huge release. There is so much more in
terms of data analysis. There is also better MIDI file support. There
are more functions for tidy analysis. Several internal and user-facing
functions have been vectorized. Other functions have been generalized
that previously lacked an approach to chords in certain contexts. The
entire package has been significantly optimized for better performance
over previous versions.

More can be done with music notation syntax converters, moving to and
from data frames, and between classes. New classes `noteinfo` and
`music` have been added along with accompanying functions. Support for
triplets have been integrated into `noteinfo` using the `t`-prefix
notation, which is supported in turn now by `music` and by `phrase`.

Since it’s still in early versions, several aspects of the package and
its approaches to working with musical information have been reimagined,
corrected, enhanced and made more resilient while keeping to the simple
syntax and premises.

All things considered, the next release of `tabr` is the one to use. You
can get many of these developments now by installing version 0.3.9.9000
from GitHub, which all the current online documentation is updated for
(this document, the `pkgdown` website, help files and vignettes).

Install the CRAN release of `tabr` with

``` r
install.packages("tabr")
```

Install the development version from GitHub with

``` r
# install.packages("remotes")
remotes::install_github("leonawicz/tabr")
```

## Noteworthy strings

As a quick introduction and to get oriented to the music notation syntax
offered by `tabr`, consider the concept of a noteworthy string. This is
like any other character string, except that what makes a string
noteworthy is that its content consists strictly of valid `tabr` music
notation syntax. It can be parsed unambiguously and meaningfully into a
musical phrase (see next section) and can be processed as input to the
various package functions that inspect and manipulate musical
information.

A simple character string like `"c e g"`, or alternatively as a vector,
`c("c", "e", "g")`, is a noteworthy string. The single lowercase letter
`"a"` is noteworthy. So are `"a_"` and `"a#"` (flat and sharp). However,
`"A"` is not, nor is `"z"`. There are other pieces of valid syntax than
just the lowercase letters `a` through `g` and sharp and flat notation.
The most important for specifying pitch is to indicate the octave
number, either in tick (recommended; comma and single quote) or integer
format (not recommended, more limited utility). For all the available
syntax specifications and related details see the package vignettes.

Noteworthiness can be checked on any character string. When defining
noteworthy strings you can define them like any other character vector.
However, you will notice that package functions that operate on
noteworthy strings and whose output is another noteworthy string will
yield a string with the supplemental `noteworthy` class. This has its
own print and summary methods. Several other generic methods are also
implemented. While many functions will attempt to coerce a string to
`noteworthy`, not all will and some methods are implemented specifically
for the class.

``` r
x <- "g#, c d# g#c'd#'"
as_noteworthy(x)
#> <Noteworthy string>
#>   Format: space-delimited time
#>   Values: g#, c d# <g#c'd#'>

x <- "g#2 c d# g#c4d#4" # equivalent octave numbering
as_noteworthy(x)
#> <Noteworthy string>
#>   Format: space-delimited time
#>   Values: g#2 c d# <g#c4d#4>

is_note(x)
#> [1]  TRUE  TRUE  TRUE FALSE
is_chord(x)
#> [1] FALSE FALSE FALSE  TRUE
chord_is_major(x)
#> [1]   NA   NA   NA TRUE
(x <- transpose(x, 1))
#> <Noteworthy string>
#>   Format: space-delimited time
#>   Values: a2 c# e <ac#4e4>

summary(x)
#> <Noteworthy string>
#>   Timesteps: 4 (3 notes, 1 chord)
#>   Octaves: integer
#>   Accidentals: sharp
#>   Format: space-delimited time
#>   Values: a2 c# e <ac#4e4>

distinct_pitches(x)
#> <Noteworthy string>
#>   Format: space-delimited time
#>   Values: a2 c# e a c#4 e4
distinct_pitches(x) %>% pitch_freq() # in Hz
#> [1] 110.0000 138.5913 164.8138 220.0000 277.1826 329.6276
```

There is also a `noteinfo` class and a `music` class. You can learn more
in the vignettes.

## Tidy music analysis

Ideally music data already exists in a data frame format. But if it
doesn’t, or if you just wrote out a new note sequence like below,
getting this data into a data frame for a more tidy approach to analysis
is easy. Conversion can also populate several derivative variables in
the process.

In the earlier example you saw the result of calling `as_music_df` on a
noteworthy string.

``` r
x <- "a, c e r r c a, g#, a ac'e'"
as_music_df(x)
```

You may have noticed that rests (`r`) are allowed for timesteps and that
functions that compute lagged intervals respect these gaps. Since all
that was provided to `as_music_df` was a string of pitches, there are no
time variables in the current data frame. However, discrete timesteps
still exist and they do not have to contain notes.

There are a number of derivative columns. If you are working with a
large sequence of music, there is no need to carry all of these along
through your analysis if you do not need them. They can be created using
various package functions and you can build onto your data frame and
transform variables later with `mutate`.

``` r
library(dplyr)
x <- "a, c e r r c a, g#, a ac'e'"
tibble(pitch = as_vector_time(x)) %>% 
  mutate(scale_int = scale_diff(pitch))
#> # A tibble: 10 x 2
#>    pitch      scale_int
#>    <notwrthy> <chr>    
#>  1 a,         <NA>     
#>  2 c          m3       
#>  3 e          M3       
#>  4 r          <NA>     
#>  5 r          <NA>     
#>  6 c          M3       
#>  7 a,         m3       
#>  8 g#,        m2       
#>  9 a          m9       
#> 10 ac'e'      P1
```

In fact, it’s much more powerful to create the columns according to your
needs using specific functions and their various arguments. But
`as_music_df` is convenient and also offers some additional arguments.
Adding `key` and `scale` allows for scale degrees. `scale` is diatonic
by default but does not have to be.

``` r
x <- "g g#"
as_music_df(x, key = "am") %>% 
  select(pitch, key, scale, scale_deg)
#> # A tibble: 2 x 4
#>   pitch key   scale    scale_deg
#>   <chr> <chr> <chr>        <int>
#> 1 g     am    diatonic         7
#> 2 g#    am    diatonic        NA

as_music_df(x, key = "am", scale = "harmonic_minor") %>% 
  select(pitch, key, scale, scale_deg)
#> # A tibble: 2 x 4
#>   pitch key   scale          scale_deg
#>   <chr> <chr> <chr>              <int>
#> 1 g     am    harmonic_minor        NA
#> 2 g#    am    harmonic_minor         7
```

`tabr` offers many functions for manipulating and analyzing music data
and working in music notation. See the collection of vignettes for more
information on music programming and analysis.

## Basic transcription example

Rendering sheet music is based on building up pieces of musical
information culminating in a score. The fundamental object to consider
in the transcription context is a phrase. A phrase is created from a
noteworthy string and incorporates additional information, most
importantly time and rhythm. It can also include positional information
such as the instrument string on which a note is played. Outside of
rendering tabs, there is no reason to construct phrase objects.
Everything from the phrase object on up is about using the R to LilyPond
pipeline to render some kind of sheet music document.

If you are doing music analysis on noteworthy strings and are combining
the note, pitch or chord information with time, that can be done with a
corresponding variable; using a phrase object is not the way to do that
because phrase objects are intended for the construction of LilyPond
markup syntax.

As a brief example, recreate the tablature shown in the image above
(minus the R logo). Here are the steps.

  - Define a musical phrase with `phrase` or the shorthand alias `p`.
  - Add the phrase to a `track`.
  - Add the track to a `score`.
  - Render the score to pdf with `tab`.

The code is shown below, but first some context.

### Constructing a musical phrase

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
specifications are covered in the vignette tutorials.

The third argument, `string`, is optional but very important for
accurate guitar tablature. For general sheet music, this argument can be
ignored. In similar format, it specifies the strings of the guitar (or
other stringed, fretted instrument) on which notes are played. Providing
this information fixes the fret-string combinations so that LilyPond
does not have to guess what position on the neck of the guitar to play a
specific note. An inability to specify this in various tablature
notation software (or laziness by the user), is a common cause of
inaccurate tabs scouring the internet, where even when the notes are
correct they are written in the tab suggesting they be played in
positions no one would sensibly use. Note that the `x` shown below is
just a placeholder indicating no need to specify a string for the
quarter note rest.

The example below employs a couple shortcuts to further reduce typing.
The first is to use the `*` in-string expansion operator mentioned above
to avoid typing a long series of eighth notes. Second, it drops explicit
reference to octave number three since octave 3 is the default octave in
LilyPond where it similarly does not need to be written explicitly. This
applies to all but the first note below.

While explicit string numbers are not needed for this example, they are
provided anyway for full context. Dropping the `string` argument would
further reduce typing.

This is the general approach, but there are multiple ways to create
equivalent phrase objects in `tabr`.

### Score metadata and accessing LilyPond

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

### Minimal R code example

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

The pdf output looks like this:

<p>

<img src="https://github.com/leonawicz/tabr/blob/master/data-raw/vignette-pngs/ex00.png?raw=true" class="centerimg" width="50%">

</p>

For comparison, if you use string-fret specification to construct the
above phrase, one way to do so is the following.

``` r
sfp("r;r;4 5;0;8 3 4;3; 0 3;2; 4;3;")
#> <Musical phrase>
#> r4 <a,\5>8 <c\5>8 <f\4>8 <d\4>8 <a\3>8 <f\4>8
```

It may not look particularly beneficial here, but for more complex music
it can be easier to reason about the phrase under construction when
using this format to bind information by time step rather by information
type. See `?sf_phrase` for a comparison with `phrase` and the various
ways you can do phrase construction in `tabr` for equivalent results. If
you are looking to do quick, easy and basic tabbing, you may want to
consider using the single-argument input method of the `sf_phrase`
function. The package vignettes focus on general use cases using the
`phrase` function rather than `sf_phrase`.

Note above that `tabr` also exports the pipe `%>%` operator. Even given
the hierarchy of objects involved in the series of steps to move from a
phrase to a rendered pdf, a short example like this does not even
require a single assignment. While music can be quite complex and a full
score will be much longer, `tabr` strives to minimize the work while
still forcing some sense of interpretable, organized structure. For long
and complex music, it can require some effort and practice to ensure
your approach to transcription in your R code is not opaque.

## References and resources

There are several vignette tutorials and examples at the `tabr`
[website](https://leonawicz.github.io/tabr/).

<img src="https://raw.githubusercontent.com/r-music/site/master/img/logo.png" style="float:left;margin-right:20px;" width="120">

<div>

<h3 style="padding-top:50px;">

R-Music

</h3>

<h4 style="padding:0px;margin-top:5px;margin-bottom:5px;">

R for music data extraction and analysis

</h4>

See the <a href="https://github.com/r-music">R-Music</a> organization on
GitHub for more R packages related to music data extraction and
analysis.<br/> The R-Music <a href="https://r-music.rbind.io/">blog</a>
provides package introductions and examples.

</div>

### Other packages

  - The [tuneR](https://CRAN.R-project.org/package=tuneR) package for
    analysis of music and speech by Uwe Ligges, Sebastian Krey, Olaf
    Mersmann, and Sarah Schnackenberg.

-----

Please note that the `tabr` project is released with a [Contributor Code
of Conduct](https://leonawicz.github.io/tabr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
