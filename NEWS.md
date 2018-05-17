# tabr 0.2.0

* Refactored `tuplet` (and `triplet`) to accept a phrase object as well as a character string of notes. Previously, only notes were accepted but this was too limiting. The argument name has changed to from `notes` to `x` and `tuplet` will now check the class of `x` and handle phrase objects accordingly.
* Added handling of silent rests in note strings for `tuplet`.
* `sf_phrase` (and `sfp`) updated to allow returning early with only the notes string as opposed to the entire phrase object. This is useful if you just want a quick, cleaner string representation of what notes are mapped by string/fret combinations.
* Minor updates to `.onLoad` for non-Windows systems.
* Removed `tibble` package dependency. Using only `dplyr` suffices.
* Updated Readme and basic example intro vignette.
* Added and updated unit tests.
* Added `lintr` to Suggests field in DESCRIPTION per CRAN maintainer request regarding `testthat` unit tests.

# tabr 0.1.2 (Release date: 2018-04-18)

* Unwrapped `lilypond` example from `dontrun` tag.
* Switched to `tempdir()` location for examples that write files.
* Added LilyPond to SystemRequirements field in DESCRIPTION.
* Attempt to run file-writing examples and file-writing unit tests conditionally if LilyPond found on system.

# tabr 0.1.1

Adjustments to meet requirements for CRAN resubmission: 

* Updated DESCRIPTION.
* Adjusted file-writing locations for unit tests to use `tempdir`.
* Add `dontrun` tag around one last file-writing package example that was using `lilypond`.
* Update `lilypond`, `tab`, `midily` and `miditab` to work with system calls that use absolute paths for output files instead of only working with relative paths.

# tabr 0.1.0

* Added package scaffolding.
* Developed initial functions for music description and organization.
* Developed initial functions for generating Lilypond (`.ly`) files and wrapping around system calls to Lilypond for rendering sheet music to pdf or png.
* Added readme with basic example.
* Added initial vignette content.
* Added unit tests.
* Added support for custom, non-standard guitar tunings and instruments with different numbers of strings, up to seven.
* Added `.mid` to `.ly` and `.mid` to tab output convenience functions.
* Added support for multiple voices per staff.
* Added support for transposition of music staff relative to tablature staff.
