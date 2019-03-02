library(dplyr)
library(tabr)
source("data-raw/chord_input1.R")
source("data-raw/chord_input2.R")

f <- function(x, d, key){
  purrr::map_dfr(seq_along(d$frets), ~chord_def(d$frets[[.x]] + x, d$id[.x], d$opt[[.x]], key = key))
}

guitarChords <- bind_rows(
  purrr::map_dfr(-1:11, ~f(.x, chordData1, "g")),
  purrr::map_dfr(-1:11, ~f(.x, chordData1, "f")),
  purrr::map_dfr(-1:11, ~f(.x, chordData2, "g")),
  purrr::map_dfr(-1:11, ~f(.x, chordData2, "f"))
) %>% select(-optional) %>% filter(!duplicated(.))

id_levels <- c("M", "m", "7", "M7", "m7",
  "sus2", "sus4", "aug", "aug7", "dim", "dim7", "7_5", "7#5", "M7_5", "M7#5", "m7_5", "m7#5",
  "9", "M9", "m9", "add9","9sus4", "9#5",
  "6", "m6", "m_6", "6add9", "m6add9",
  "add2", "madd2", "madd4",
  "mM7",
  "7sus2", "7sus4", "7_9", "7#9", "m7_9", "7_9_5", "7_9#5", "7#9_5",
  "7#9_13", "7#11", "7_13", "M7#5#11", "M711", "M7#11", "M713", "M7913",
  "9#11", "11", "M11", "m11", "13", "M13", "m13", "13sus4", "13_5", "13#5", "13#11")

guitarChords <- mutate(guitarChords, id = factor(id, levels = id_levels)) %>%
  arrange(root, octave, root_fret, id)

usethis::use_data(guitarChords, overwrite = TRUE)
