# tabr 0.1.2

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
