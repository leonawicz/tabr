.predefined_tunings <- c(
  standard = "e, a, d g b e'", seven = "b,, e, a, d g b e'",
  dropD = "d, a, d g b e'", dropC = "c, g, c f a d'",
  openG = "d, g, d g b d'", openD = "d, a, d fis a d'",
  DADGAD = "d, a, d g a d'", lute = "e, a, d fis b e'",
  asus4 = "e, a, d e a e'",
  bass = "e,, a,, d, g,", bassdropD = "d,, a,, d, g,",
  bass5 = "b,,, e,, a,, d, g,", bass6 = "b,,, e,, a,, d, g, c",
  mandolin = "g d' a' e''",
  banOG = "g' d g b d'", banC = "g' c g b d'",
  banModal = "g' d g c' d'", banOD = "a' d fis a d'",
  banODm = "a' d fis a d'",
  ban4OG = "g' d g b d'", ban4C = "g' c g b d'",
  ban4Modal = "g' d g c' d'", ban4OD = "a' d fis a d'",
  ban4ODm = "a' d fis a d'",
  uke = "g' c' e' a'", ukeD = "a' d' fis' b'",
  ukeTenor = "g c' e' a'", ukeBaritone = "d g b e'",
  violin = "g d' a' e''", viola = "c g d' a'",
  cello = "c, g, d a", doublebass = "e,, a,, d, g,"
)

tunings <- data.frame(id = names(.predefined_tunings), value = .predefined_tunings,
                      row.names = NULL, stringsAsFactors = FALSE)

.syntax_desc <- c("note/pitch", "sharp", "flat", "drop or raise one octave", "octave number", "tied notes", "note duration", "dotted note", "slide", "bend", "staccato", "muted/dead note", "slur/hammer/pull off", "rest", "silent rest", "expansion operator")
.syntax_id <- c("a b ... g", "#", "_", ", or '", "0 1 ...", "~", "2^n", ".", "-", "^", "]", "x", "()", "r", "s", "*")
.syntax_example <- c("a", "a#", "a_", "a, a a'", "a2 a3 a4", "a~ a", "1 2 4 8 16", "2. 2..", "2-", "2^", "2]", "2x", "2( 2)", "r", "s", "ceg*8, 1*4")
tabrSyntax <- data.frame(description = .syntax_desc, syntax = .syntax_id, example = .syntax_example)

mainIntervals <- data.frame(
  semitones = 0:25,
  mmp = c("perfect unison", "minor second", "major second", "minor third", "major third", "perfect fourth",
          "tritone", "perfect fifth", "minor sixth", "major sixth", "minor seventh", "major seventh",
          "perfect octave", "minor ninth", "major ninth", "minor tenth", "major tenth", "perfect eleventh",
          NA, "perfect twelfth", "minor thirteenth", "major thirteenth", "minor fourteenth", "major fourteenth",
          "perfect fifteenth", NA),
  mmp_abb = c("P1", "m2", "M2", "m3", "M3", "P4", "TT", "P5", "m6", "M6", "m7", "M7", "P8",
              "m9", "M9", "m10", "M10", "P11", NA, "P12", "m13", "M13", "m14", "M14", "P15", NA),
  ad = c(
    paste(c("diminished", "augmented"), c("second", "unison", "third", "second", "fourth", "third")),
    "diminished fifth/augmented fourth",
    paste(c("diminished", "augmented"), c("sixth", "fifth", "seventh", "sixth", "octave")),
    "diminished ninth/augmented seventh",
    paste(c("augmented", "diminished"), c("octave", "tenth", "ninth", "eleventh", "tenth")),
    "diminished twelfth/augmented eleventh",
    paste(c("diminished", "augmented"), c("thirteenth", "twelfth", "fourteenth", "thirteenth", "fifteenth",
                                          "fourteenth")),
    "augmented fifteenth"),
  ad_abb = c(
    paste0(c("d", "A"), c(2, 1, 3, 2, 4, 3)), "d5/A4",
    paste0(c("d", "A"), c(6, 5, 7, 6, 8)), "d9/A7",
    paste0(c("A", "d"), c(8, 10, 9, 11, 10)), "d12/A11",
    paste0(c("d", "A"), c(13, 12, 14, 13, 15, 14)), "A15"),
  stringsAsFactors = FALSE
)

usethis::use_data(tunings, tabrSyntax, mainIntervals)

library(tabr)
.all_pitches <- sapply(0:131, function(x) transpose("c,,,,", x, "flat", "tick"))

usethis::use_data(.all_pitches, internal = TRUE)
