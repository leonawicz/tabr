# tabr 0.3.9.9000

* Refactored many package functions. Most behavior unchanged, but some breaking changes were made based on intentional shifts in perspective about what some functions should do and how the user should interact with them. This included changes to function arguments as well as some redefinition of what certain functions do.
* Substantial code optimization was done, overhauling much of the package in the process.
* Vectorized functions that previously only operated note by note.
* Generalized functions that previously could accept notes but not chords, doing so on a case by case basis in ways that are meaningful and sensible for handling chords.
* Added new classes `noteinfo` to complement `noteworthy` and associated functions.
* Added new class `music` which builds upon the combination of `noteworthy` and `noteinfo`, similar in content to `phrase` but maintaining the structure of the other `tabr` classes for data manipulation and analysis.
* Added implementations for several common R functions including primitives like `c`, `length`, `[` and more to be used with special classes available in `tabr`.
* Refactored some basic metadata functions as generics to dispatch to the new classes rather than only working for `noteworthy` objects. For example, `time_format`.
* Added native triplet support in note info using the `t`-prefix, e.g., `4 4] t8 t8- t8^ 4`. Support extends to `music` objects and now also to `phrase`, which alleviates reliance on the `triplet` function and its limitations.
* Added MIDI file read support (requires optional `tuneR` installation) and a set of functions for inspecting and manipulating the table of MIDI music data.
* Added more functions for music data manipulation and analysis.
* Added more functions for mapping between noteworthy strings, phrase objects, and data frames.
* Added syntax converters `from_chorrrds` (for chord output from `chorrrds` package) and `from_music21`, for converting other music notation syntax to `tabr` syntax.
* Added `track_*` wrapper functions to provide better default track arguments for different instruments and use cases.
* Added new vignettes on syntax conversion and rendering chord charts.
* Updated vignettes, readme and other documentation.
* Made improvements to print method for phrase objects.
* Added `rests` argument to some note metadata functions.
* Minor bug fixes.

# tabr 0.3.5

* Added alternate input specification for `sf_phrase`. Instead of providing the first three function arguments, `string`, `fret` and `info`, separately, you can now provide everything to the first input `string` as a single character string containing all three components separated by semicolons. This makes it easier to reason about the input by time step rather than by argument.
* Added chord helpers: `chord_root`, `chord_top`, `chord_slice`, `chord_is_major`, `chord_is_minor`.
* Added notation-frequency conversion helpers: `pitch_freq`, `freq_pitch` and other related functions.
* Added several more functions for inspecting and manipulating noteworthy strings.
* Code and documentation formatting and style overhaul based on stricter linting rules.
* Updated documentation and unit tests.

# tabr 0.3.1

* Added `no_tab` argument to `track` allowing for suppression of tab staff when music staff is included, e.g., for vocal tracks.
* `no_tab`-associated documentation and unit test updates.
* Deprecated `dup` and `glue`. Now use `pn` for repeating phrases `n` times and `pc` for concatenating multiple phrases.
* Updated documentation, examples, vignettes and unit tests.
* Minor bug fixes.

# tabr 0.3.0

* Added `fretboard_plot` for making fretboard diagrams outside of the LilyPond tablature pipeline. This necessitates importing `ggplot2`.
* Added functions for phrase validation, coercion and decomposition back to original character string inputs.
* Added functions for note, pitch, chord and octave equivalence checks.
* Added a collection of functions for constructing and working with common chords. The chord constructors are among the `chord_*`-named functions and also have shorter `x*`-named aliases. These are "piano chords," i.e., based on the condensed, defining intervals.
* Added `guitarChords` dataset containing several thousand formations of guitar chord voicings.
* Added helper functions that use `guitarChords` for obtaining guitar chord information and mapping between different defining properties of a chord, most notably the addition of `gc_notes` and `gc_fretboard` for mapping chord names to noteworthy strings and fretboard diagram syntax.
* Added helper functions for working with basic note/pitch strings.
* Added helper functions for working with musical scales and modes.
* Added helper functions for working with musical intervals.
* Added `noteworthy` class, used internally, optional for users (includes custom print and summary methods).
* Added `mainIntervals` dataset.
* Added more internal checks of note and chord syntax validity across functions that work with string representations (pre-`phrase` object construction).
* Update older functions to utilize the new, more robust and stricter validation checks and offer more consistent `noteworthy` class output.
* updated `transpose` to handle additional edge cases, including a new style option, `strip`.
* Updated and added new unit tests.
* Bug fix for case where `NA`-valued no-chord rests (`s` or `r`) were unnamed in output of `chord_set`.
* Fixed entry in `tabrSyntax`.
* Fix class assignment bug and updated `as_phrase`.
* Added a new column of relative interval size to internal `.keydata` helper table.
* Added new vignettes focusing on the programming aspect of `tabr`.
* Updated documentation.

# tabr 0.2.0

* Refactored `tuplet` (and `triplet`) to accept a phrase object as well as a character string of notes. Previously, only notes were accepted but this was too limiting. The argument name has changed to from `notes` to `x` and `tuplet` will now check the class of `x` and handle phrase objects accordingly.
* Added handling of silent rests in note strings for `tuplet`.
* `sf_phrase` (and `sfp`) updated to allow returning early with only the notes string as opposed to the entire phrase object. This is useful if you just want a quick, cleaner string representation of what notes are mapped by string/fret combinations.
* Minor updates to `.onLoad` for non-Windows systems.
* Removed `tibble` package dependency. Using only `dplyr` suffices.
* Updated Readme and basic example intro vignette.
* Added and updated unit tests.
* Added `lintr` to Suggests field in DESCRIPTION per CRAN maintainer request regarding `testthat` unit tests.

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
* Developed initial functions for generating LilyPond (`.ly`) files and wrapping around system calls to LilyPond for rendering sheet music to pdf or png.
* Added readme with basic example.
* Added initial vignette content.
* Added unit tests.
* Added support for custom, non-standard guitar tunings and instruments with different numbers of strings, up to seven.
* Added `.mid` to `.ly` and `.mid` to tab output convenience functions.
* Added support for multiple voices per staff.
* Added support for transposition of music staff relative to tablature staff.
