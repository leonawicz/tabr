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
.syntax_example <- c("a", "a#", "a_", "a, a a'", "a2 a3 a4", "a~ a", "1 2 4 8 16", "2. 2..", "2-", "2^", "2[", "2x", "2( 2)", "r", "s", "ceg*8, 1*4")
tabrSyntax <- data.frame(description = .syntax_desc, syntax = .syntax_id, example = .syntax_example)

usethis::use_data(tunings, tabrSyntax)
