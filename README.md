
<!-- README.md is generated from README.Rmd. Please edit that file -->
tabr
====

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/tabr.svg?branch=master)](https://travis-ci.org/leonawicz/tabr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/tabr?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/tabr) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/tabr/master.svg)](https://codecov.io/github/leonawicz/tabr?branch=master)

Create guitar tablature ("tabs") from R code with `tabr`. The `tabr` package provides programmatic music notation and wraps around the open source music engraving program, Lilypond, for creating quality guitar tablature.<img src="tabr_logo_small.png" style="float:right;margin:5px;width:400px;">

`tabr` offers functions for describing and organizing musical structures and a wrapper around the Lilypond backend. Lilypond is open source music engraving software for generating high quality sheet music based on markup language. `tabr` generates files following the Lilypond markup syntax to be subsequently processed by Lilypond into sheet music pdf files.

A standalone Lilypond (.ly) file can be created or the package can make a system call to Lilypond directly to render the guitar tablature output (.pdf). While Lilypond caters to sheet music in general, `tabr` is focused on leveraging it specifically for creating quality guitar tablature.

Installation
------------

You can install tabr from GitHub with:

``` r
# install.packages('devtools')
devtools::install_github("leonawicz/tabr")
```
