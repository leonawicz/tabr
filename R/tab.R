#' Save score to LilyPond file
#'
#' Write a score to a LilyPond format (\code{.ly}) text file for later use by LilyPond or subsequent editing outside of R.
#'
#' All \code{header} list elements are character strings. The options for \code{header} include:
#' \itemize{
#'   \item \code{title}
#'   \item \code{subtitle}
#'   \item \code{composer}
#'   \item \code{album}
#'   \item \code{arranger}
#'   \item \code{instrument}
#'   \item \code{meter}
#'   \item \code{opus}
#'   \item \code{piece}
#'   \item \code{poet}
#'   \item \code{copyright}
#'   \item \code{tagline}
#' }
#'
#' All \code{paper} list elements are numeric except \code{page_numbers}, which is logical. The options for \code{paper} include:
#' \itemize{
#'   \item \code{textheight}
#'   \item \code{linewidth}
#'   \item \code{indent}
#'   \item \code{first_page_number}
#'   \item \code{page_numbers}
#'   \item \code{fontsize}
#' }
#'
#' @param score a score object.
#' @param file character, LilyPond output file ending in \code{.ly}.
#' @param key character, key signature, e.g., \code{c}, \code{b_}, \code{f#m}, etc.
#' @param time character, defaults to \code{"4/4"}.
#' @param tempo character, defaults to \code{"2 = 60"}.
#' @param header a named list of arguments passed to the header of the LilyPond file. See details.
#' @param string_names label strings at beginning of tab staff. \code{NULL} (default) for non-standard tunings only, \code{TRUE} or \code{FALSE} for force on or off completely.
#' @param paper a named list of arguments for the LilyPond file page layout. See details.
#' @param endbar character, the end bar.
#' @param midi logical, add midi inclusion specification to LilyPond file.
#' @param path character, output directory for \code{file}, must be a relative path.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso \code{\link{tab}}, \code{\link{midily}},
#'
#' @examples
#' x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#' x <- track(x)
#' x <- score(x)
#' lilypond(x, "out.ly")
lilypond <- function(score, file, key = "c", time = "4/4", tempo = "2 = 60", header = NULL,
                     string_names = NULL, paper = NULL, endbar = TRUE, midi = TRUE, path = "."){
  if(!"score" %in% class(score)) stop("`score` is not a score object.")
  major <- ifelse(utils::tail(strsplit(key, "")[[1]], 1) == "m", FALSE, TRUE)
  raw_key <- gsub("m", "", key)
  key <- .notesub(raw_key)
  mode <- ifelse(major, "\\major", "\\minor")
  if((major && !key %in% .keys$major) || (!major && !key %in% .keys$minor)) stop("Invalid key.")
  paper_args <- .lp_paper_args(paper)
  paper <- do.call(.lp_paper, paper_args)
  chords <- attributes(score)$chords
  has_chords <- !is.null(chords)
  chord_seq <- attributes(score)$chord_seq
  if(has_chords){
    names(chords) <- .notesub(names(chords))
    names(chord_seq) <- .notesub(names(chord_seq))
  }
  global <- .lp_global(time, key, mode, tempo, endbar)
  rel_tp <- ifelse(any(score$ms_transpose != 0), TRUE, FALSE)
  top <- .lp_top(paper_args$fontsize, header, rel_tp)
  if(has_chords){
    top <- paste0(top, purrr::map_chr(
      seq_along(chords), ~paste0("#(define fb", .x, " (make-fretboard-table))\n",
                                 "\\storePredefinedDiagram #fb", .x, " \\chordmode{",
                                 names(chords)[.x], "} #guitar-tuning \"", chords[[.x]], "\"\n")) %>%
        paste(collapse = ""), "\n", collapse = "")
    cd <- .chord_diagram(chords, chord_seq)
  } else {
    cd <- NULL
  }
  d <- split(score, score$tabstaff)
  melody0 <- split(score$phrase, score$tabstaff)
  melody_id <- paste0("melody", LETTERS[seq_along(melody0)])
  melody <- paste0(purrr::map_chr(seq_along(d), ~.set_melody(melody0[[.x]], d[[.x]], melody_id[.x])), collapse = "")
  melody_id_final <- .get_melody_id(melody)
  score <- .set_score(d, melody_id, TRUE, NULL, NULL, tempo, has_chords, string_names, rel_tp, raw_key)

  midi_tag <- paste0("  \\midi{\n    \\tempo ", tempo, "\n  }\n", collapse = "")
  midi_melody <- NULL
  if(midi){
    any_repeats <- any(purrr::map_int("repeat", ~length(grep(.x, melody))))
    if(any_repeats){
      melody_id2 <- paste0("midi", melody_id) # nolint
      midi_melody <- paste0(purrr::map_chr(seq_along(d),
                                           ~.set_melody(melody0[[.x]], d[[.x]], melody_id2[.x], TRUE)), collapse = "")

      melody_id2_final <- .get_melody_id(midi_melody)
      score2 <- .set_score(d, melody_id, FALSE, midi_tag, melody_id2_final, tempo, FALSE, NULL, rel_tp, raw_key)
      melody <- paste0(melody, midi_melody, collapse = "\n\n")
    } else {
      score2 <- .set_score(d, melody_id, FALSE, midi_tag, melody_id_final, tempo, FALSE, NULL, rel_tp, raw_key)
    }
    score <- paste0(score, score2, collapse = "\n")
  }
  output <- paste(c(top, global, cd, melody, score, paper), collapse = "")
  write(file = file.path(path, file), output)
}

# nolint start

.get_melody_id <- function(x){
  x <- strsplit(x, "[ =\n]")[[1]]
  idx <- grep("melody", x)
  x[idx]
}

#' Create tablature
#'
#' Create sheet music/guitar tablature from a music score.
#'
#' Generate a pdf or png of a music score using the LilyPond music engraving program.
#' Output format is inferred from \code{file} extension. This function is a wrapper around \code{\link{lilypond}}, the function that creates the LilyPond (\code{.ly}) file.
#'
#' @param score a score object.
#' @param file character, output file ending in .pdf or .png.
#' @param key character, key signature, e.g., \code{c}, \code{b_}, \code{f#m}, etc.
#' @param time character, defaults to \code{"4/4"}.
#' @param tempo character, defaults to \code{"2 = 60"}.
#' @param header a named list of arguments passed to the header of the LilyPond file. See details.
#' @param string_names label strings at beginning of tab staff. \code{NULL} (default) for non-standard tunings only, \code{TRUE} or \code{FALSE} for force on or off completely.
#' @param paper a named list of arguments for the LilyPond file page layout. See details.
#' @param endbar character, the end bar.
#' @param midi logical, output midi file in addition to tablature.
#' @param keep_ly logical, keep LilyPond file.
#' @param path character, output directory for \code{file}, must be a relative path.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso \code{\link{lilypond}}, \code{\link{miditab}}
#'
#' @examples
#' x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#' x <- track(x)
#' x <- score(x)
#' \dontrun{tab(x, "out.ly") # requires LilyPond installation}
tab <- function(score, file, key = "c", time = "4/4", tempo = "2 = 60", header = NULL,
                string_names = NULL, paper = NULL, endbar = TRUE, midi = TRUE,
                keep_ly = FALSE, path = "."){
  ext <- utils::tail(strsplit(file, "\\.")[[1]], 1)
  lily <- gsub(ext, "ly", file)
  cat("#### Engraving score to", file, "####\n")
  lilypond(score, lily, key, time, tempo, header, string_names, paper, endbar, midi, path)
  lily <- file.path(path, lily)
  system(paste0("\"", tabr_options()$lilypond, "\" --", ext,
                " -dstrip-output-dir=#f \"", lily, "\""))
  if(!keep_ly) unlink(lily)
  invisible()
}

.lp_header <- function(title = "", subtitle = "", composer = "", arranger = "", instrument = "",
                       meter = "", opus = "", piece = "", poet = "", copyright = "", tagline = "", ...){
  x <- list(...)
  if(!is.null(x$metre)) meter <- x$metre
  if(!is.null(subtitle) & !is.null(x$album)){
    subtitle <- paste("\\markup {", gsub(x$album, paste0("\\\\italic \"", x$album, "\""), subtitle), "}")
  } else {
    subtitle <- paste0("\"", subtitle, "\"", collapse = "")
  }
  header <- paste0(
    "\\header {\n", "  title = \"", title, "\"\n", "  subtitle = ", subtitle, "\n",
    "  composer = \"", composer, "\"\n", "  arranger = \"", arranger, "\"\n",
    "  instrument = \"", instrument, "\"\n", "  metre = \"", meter, "\"\n",
    "  opus = \"", opus, "\"\n", "  piece = \"", piece, "\"\n",
    "  poet = \"", poet, "\"\n", "  copyright = \"", copyright, "\"\n",
    "  tagline = \"", tagline, "\"\n}\n")
}

# nolint end

.paper_defaults <- list(textheight = 220, linewidth = 150, indent = 0, fontsize = 14,
                        page_numbers = TRUE, first_page_number = 1)

.keys <- list(
  major = c("fis", "h", "e", "a", "d", "g", "c", "f", "b", "es", "as", "des", "ges"),
  minor = c("cis", "gis", "dis", "fis", "h", "e", "a", "d", "g", "c", "f", "b", "es")
)

# nolint start

.set_melody <- function(x, d, id, midi = FALSE, rel_tp = FALSE, raw_key){
  multivoice <- length(unique(d$voice)) > 1
  if(!multivoice) x0 <- paste0(id, " = {\n  \\global\n  ")
  if(multivoice){
    x <- split(x, d$voice)
    x0 <- paste0(id, LETTERS[as.integer(names(x))], " = {\n  \\global\n  ")
    v <- c("One", "Two")
    x <- purrr::map2(x, seq_along(x), ~paste0("  \\voice", v[.y], " ",
                                              paste(.x, collapse = " "), "\n")) %>% unlist()
    if(midi) x <- paste("\\unfoldRepeats {", x, "}")
    x <- paste0(x0, "\\override StringNumber #'transparent = ##t\n  ", gsub("\n\n", "\n", gsub("\\|", "\\|\n", x)), "}\n\n", collapse = "\n")
  } else {
    x <- paste0(paste0(x, collapse = ""), "\n")
    if(midi) x <- paste("\\unfoldRepeats {", x, "}")
    x <- paste0(x0, "\\override StringNumber #'transparent = ##t\n  ", gsub("\n\n", "\n", gsub("\\|", "\\|\n", x)), "}\n\n")
  }
  gsub("  ", "", gsub("  ", " ", x))
}

# nolint end

.lp_paper_args <- function(x){
  if(is.null(x)) return(.paper_defaults)
  for(i in names(.paper_defaults)) if(!i %in% names(x)) x[[i]] <- .paper_defaults[[i]]
  x
}

.lp_paper <- function(...){
  x <- list(...)
  pn <- ifelse(x$page_numbers, "##t", "##f")
  paste0(
    "\\paper{\n",
    paste0("    textheight = ", x$textheight, ".\\mm\n"),
    paste0("    linewidth = ", x$linewidth, ".\\mm\n"),
    paste0("    indent = ", x$indent, ".\\mm\n"),
    paste0("    first-page-number = ", x$first_page_number, "\n"),
    paste0("    print-page-number = ", pn, "\n"),
    paste0("    print-first-page-number = ", pn, "\n"), "}"
  )
}

.lp_global <- function(time, key, mode, tempo, endbar){
  paste0("global = {\n  \\time ", time, "\n  \\key ", key, " ", mode, "\n  \\tempo ", tempo,
         if(endbar) "\n  \\bar \"|.\"", "\n}\n\n")
}

# nolint start

.lp_top <- function(fontsize, header, rel_tp){
  if(is.null(header)) header <- list()
  paste0(if(rel_tp) .ly_transpose_defs,
         paste("#(set-global-staff-size", fontsize, ")\n"), do.call(.lp_header, header),
         "\\include \"predefined-guitar-fretboards.ly\"\n\n")
}

.chord_diagram <- function(chords, chord_seq){
  diagram <- paste0("mychorddiagrams = \\chordmode {\n",
                    paste0("  \\set predefinedDiagramTable = #fb", seq_along(chords), " ",
                           names(chords), "\n", collapse = ""), "}\n\n")
  modifiers <- purrr::map_chr(strsplit(names(chord_seq), ":"), ~({if(length(.x) == 1) NA else .x[2]}))
  chords <- paste0(sapply(strsplit(names(chord_seq), ":"), "[", 1), chord_seq,
                   ifelse(is.na(modifiers), "", paste0(":", modifiers)))
  name <- paste0("chordNames = \\chordmode {\n  \\override ChordName.font-size = #2\n  \\global\n  ",
                 paste(chords, collapse = " "), "\n}\n\n")
  topcenter <- paste0(
    "\\markup\\vspace #3\n",
    "\\markup \\fill-line {\n  \\score {\n    <<\n      \\context ChordNames { \\mychorddiagrams }\n",
    "      \\context FretBoards {\n        \\override FretBoards.FretBoard.size = #'1.2\n",
    "        \\mychorddiagrams\n      }\n    >>\n  \\layout {}\n  }\n}\n\\markup\\vspace #3\n\n")
  paste0(diagram, name, topcenter, collapse = "")
}

.set_score <- function(d, id, layout, midi, midi_melody_id, tempo, has_chords, string_names, rel_tp, raw_key){
  if(rel_tp) key <- .notesub(raw_key)
  if(layout){
    clef <- purrr::map_chr(d, ~unique(.x$staff))
    tuning <- purrr::map_chr(d, ~unique(.x$tuning))
    str_lab  <- purrr::map_chr(tuning, .tunelab)
    voice <- purrr::map(d, ~unique(.x$voice))
    ms_tp <- purrr::map_int(d, ~unique(.x$ms_transpose))
    ms_key <- purrr::map_chr(d, ~unique(.x$ms_key))
    if(any(!is.na(clef))) clef <- clef[!is.na(clef)]
    x <- paste0(
      purrr::map_chr(seq_along(clef), ~({
        tp_wrap <- ms_tp[.x] != 0
        if(tp_wrap){
          rel_tp_key <- .notesub(transpose(raw_key, ms_tp[.x], ms_key[.x], "tick"))
          rel_tp_string <- paste("\\transpose", key, rel_tp_key, "{ ") # tentatively removed "\\naturalizeMusic"
        }
        multivoice <- length(voice[[.x]]) > 1
        if(!multivoice){
          x2 <- paste0("\\", id[.x])
          x1 <- paste0(if(tp_wrap) rel_tp_string, x2, if(tp_wrap) " }")
        }
        if(multivoice){
          x0 <- paste0(id, LETTERS[voice[[.x]]])
          x1 <- paste0(if(tp_wrap) rel_tp_string, "\\context Voice = \"", x0[1], "\" \\", x0[1], if(tp_wrap) " }",
                       " ", if(tp_wrap) rel_tp_string, "\\context Voice = \"", x0[2], "\" \\", x0[2], if(tp_wrap) " }")
          x2 <- paste0("\\context TabVoice = \"", x0[1], "\" \\", x0[1], " \\context TabVoice = \"", x0[2], "\" \\", x0[2])
        }
        paste0(
          if(!is.na(clef[.x]))
            paste0("\\new Staff << \\clef \"", clef[.x], "\" ", x1, " >>\n  ", collapse = ""),
          paste0("\\new TabStaff \\with { stringTunings = \\stringTuning <", .notesub(tuning[.x]), "> } <<\n    ",
                 if((is.null(string_names) && tuning[.x] != "e, a, d g b e'") || (!is.null(string_names) && string_names))
                   paste("\\set TabStaff.instrumentName = \\markup { \\hspace #7 \\override #'(baseline-skip . 1.5) \\column \\fontsize #-4.5 \\sans {",
                         str_lab[.x], "} }\n    "),
                 "\\override Stem #'transparent = ##t\n    \\override Beam #'transparent = ##t\n    ",
                 x2, "\n  >>\n  ", collapse = "")
        )
      })), collapse = "")
  } else {
    ms_tp <- unique(purrr::map_int(d, ~unique(.x$ms_transpose)))
    ms_key <- unique(purrr::map_chr(d, ~unique(.x$ms_key)))
    if(rel_tp && length(ms_tp) == 1 && ms_tp != 0 && length(ms_key) == 1){
      rel_tp_key <- .notesub(transpose(raw_key, ms_tp, ms_key, "tick"))
      rel_tp_string <- paste("\\transpose", key, rel_tp_key, "{ ") # tentatively removed "\\naturalizeMusic"
      midi_melody_id <- paste0(rel_tp_string, "\\", midi_melody_id, " }")
    } else {
      if(rel_tp) warning("Multiple music staves with different transposed key signatures. MIDI output not transposed.")
      midi_melody_id <- paste0("\\", midi_melody_id)
    }
    x <- paste0(paste0(midi_melody_id, collapse = "\n  "), "\n  ")
  }
  paste0("\\score {  <<\n  ", if(has_chords) "\\new ChordNames \\chordNames\n  ", x, ">>\n",
         if(layout) "  \\layout{ }\n", if(!is.null(midi)) midi, "}\n\n")
}

# nolint end

.tunelab <- function(x){
  x <- gsub("[,']", "", x)
  x <- toupper(x)
  x <- gsub("#", "is", x)
  x <- gsub("_", "b", x)
  x
}

.star_expand <- function(x){
  if(length(grep("\\*", x)) == 0) return(x)
  x <- strsplit(x, "\\*")[[1]]
  do.call(dup, list(x = x[1], n = as.integer(x[2])))
}

.split_chord <- function(x, strings = FALSE, abb = TRUE){
  if(nchar(x) == 1) return(x)
  y <- if(strings) c(0:9, "x", "o") else letters[1:7]
  x <- gsub("es", "ZS", x)
  if(strings){
    x <- strsplit(x, "_")[[1]]
    idx0 <- which(as.numeric(x) > 9)
    if(length(idx0)){
      xdif <- 10 * as.numeric(x)[idx0] %/% 10
      x[idx0] <- as.character(as.numeric(x)[idx0] - xdif)
    }
    x <- paste0(x, collapse = "")
  }
  idx <- which(strsplit(x, "")[[1]] %in% y)
  x <- gsub("ZS", "es", x)
  if(length(idx) == 1){
    if(!strings){
      if(abb) x <- gsub("aes", "as", gsub("ees", "es", x))
      return(x)
    }
    if(length(idx0)) x <- as.character(as.numeric(x) + xdif)
    return(x)
  }
  x <- as.character(mapply(substr, x, idx, idx + c(diff(idx), nchar(x) - utils::tail(idx, 1) + 1) - 1))
  if(!strings & abb) x <- gsub("aes", "as", gsub("ees", "es", x))
  if(strings && length(idx0)) x[idx0] <- as.character(as.numeric(x[idx0]) + xdif)
  x
}
