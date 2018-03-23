#(set-global-staff-size 14 )
\header {
  title = ""
  subtitle = ""
  composer = ""
  arranger = ""
  instrument = ""
  metre = ""
  opus = ""
  piece = ""
  poet = ""
  copyright = ""
  tagline = ""
}
\include "predefined-guitar-fretboards.ly"

global = {
  \time 4/4
  \key c \major
  \tempo 2 = 60
  \bar "|."
}

melodyA = {
  \global
  \override StringNumber #'transparent = ##t
  <c\5>4 <e\4 c'\3 g'\2>4 <e\4 c'\3 g'\2>2
}
\score {  <<
  \new Staff << \clef "treble_8" \melodyA >>
  \new TabStaff \with { stringTunings = \stringTuning <e, a, d g b e'> } <<
    \override Stem #'transparent = ##t
    \override Beam #'transparent = ##t
    \melodyA
  >>
  >>
  \layout{ }
  \midi{
    \tempo 2 = 60
  }
}

\paper{
    textheight = 220.\mm
    linewidth = 150.\mm
    indent = 0.\mm
    first-page-number = 1
    print-page-number = ##t
    print-first-page-number = ##t
}
