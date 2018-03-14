# Clone this repository

Cloning this repository may require you to remove the top-level .lintr file and regenerate a symbolic link to the actual inst/.lintr file.

The current version of this package is only set up to work on Windows. Lilypond for Windows must be installed on the system in order to use the `tab` function for creating guitar tablature output.

For the README and package vignettes, `eval=FALSE` was added to rmarkdown code chunks that wrote pdf files after being evaluated initially. They only needed to be evaluated once when calling `pkgdown::build_site` so that they could be used to screen capture and crop out small png files for the example graphics in the HTML pages. After this was done, the pdf files were removed and the relevant code chunks deactivated so as to not continue writing pdf files with subsequent website builds. In order to regenerate these source pdfs, temporarily remove the `eval=FALSE` arguments from the relevant code chunks.

There are no other notes for this respository.
