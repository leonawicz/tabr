#' Save score to LilyPond file
#'
#' Write a score to a LilyPond format (\code{.ly}) text file for later use by
#' LilyPond or subsequent editing outside of R.
#'
#' @details
#' This function only writes a LilyPond file to disk. It does not require a
#' LilyPond installation. It checks for the version number of an installation,
#' but LilyPond is not required to be found.
#'
#' This function can be used directly but is commonly used by \code{render_*}
#' functions, which call this function internally to create the LilyPond file
#' and then call LilyPond to render that file to sheet music.
#'
#' @section Header options:
#' All \code{header} list elements are character strings. The options for
#' \code{header} include:
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
#' @section Paper options:
#' All \code{paper} list elements are numeric except \code{page_numbers} and
#' \code{print_first_page_number},
#' which are logical. \code{page_numbers = FALSE} suppresses all page numbering.
#' When \code{page_numbers = TRUE}, you can set
#' \code{print_first_page_number = FALSE} to suppress printing of only the
#' first page number. \code{first_page_number} is the number of the first page,
#' defaulting to 1, and determines all subsequent page numbers. These arguments
#' correspond to LilyPond paper block variables.
#'
#' The options for \code{paper} include the following and have the following
#' default values if not provided:
#' \itemize{
#'   \item \code{textheight = 220}
#'   \item \code{linewidth = 150}
#'   \item \code{indent = 0}
#'   \item \code{fontsize = 10}
#'   \item \code{page_numbers = TRUE}
#'   \item \code{print_first_page_number = TRUE}
#'   \item \code{first_page_number = 1}
#' }
#'
#' @section PNG-related options:
#' By default \code{crop_png = TRUE}. This alters the template so that when
#' the LilyPond output file is created, it contains specifications for cropping
#' the image to the content when that file is rendered by LilyPond to png.
#' The image will have its width and height automatically cropped
#' rather than retain the standard page dimensions.
#' This only applies to png outputs made from the LilyPond file, not pdf.
#' The argument is also ignored if explicitly providing \code{textheight} to
#' \code{paper}. You may still provide \code{linewidth} to \code{paper} if you
#' find you need to increase it beyond the default 150mm, generally as a result
#' of using a large \code{fontsize}.
#' Various \code{render_*} functions that wrap \code{lilypond} make use of this
#' argument as well.
#'
#' @section Color options:
#' You can provide a named list of global color overrides for various sheet
#' music elements with the \code{colors} argument of \code{lilypond} or one of
#' the associated rendering functions.
#'
#' By default, everything is black. Overrides are only inserted into the
#' generated LilyPond file if given. Values are character; either the hex color
#' or a named R color. The named list options include:
#' \itemize{
#'   \item \code{color}
#'   \item \code{background}
#'   \item \code{staff}
#'   \item \code{time}
#'   \item \code{clef}
#'   \item \code{bar}
#'   \item \code{beam}
#'   \item \code{head}
#'   \item \code{stem}
#'   \item \code{accidental}
#'   \item \code{slur}
#'   \item \code{tabhead}
#' }
#'
#' \code{color} is a global font color for the entire score. It affects staff
#' elements and \code{header} elements. It does not affect everything, e.g.,
#' page numbers.
#' \code{background} controls the background color of the entire page. Do not
#' use this if making a transparent background png with the \code{transparent}
#' argument available in the various \code{render_*} functions.
#' The other options are also global but override \code{color}. You can change
#' the color of elements broadly with \code{color} and then change the color of
#' specific elements using the other options.
#'
#' There are currently some limitations. Specifically, if you provide any
#' \code{background} color override, most \code{header} elements will not
#' display.
#'
#' @param score a score object.
#' @param file character, LilyPond output file ending in \code{.ly}. May
#' include an absolute or relative path.
#' @param key character, key signature, e.g., \code{c}, \code{b_}, \code{f#m},
#' etc.
#' @param time character, defaults to \code{"4/4"}.
#' @param tempo character, defaults to \code{"2 = 60"}.
#' @param header a named list of arguments passed to the header of the
#' LilyPond file. See details.
#' @param paper a named list of arguments for the LilyPond file page layout.
#' See details.
#' @param string_names label strings at beginning of tab staff. \code{NULL}
#' (default) for non-standard tunings only, \code{TRUE} or \code{FALSE} for
#' force on or off completely.
#' @param endbar character, the end bar.
#' @param midi logical, add midi inclusion specification to LilyPond file.
#' @param colors a named list of LilyPond element color overrides. See details.
#' @param crop_png logical, alter template for cropped height. See
#' details.
#' @param simplify logical, uses \code{simplify_phrase} to convert to simpler,
#' more efficient LilyPond syntax.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso \code{\link{tab}}, \code{\link{render_chordchart}},
#' \code{\link{midily}}
#'
#' @examples
#' x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#' x <- track(x)
#' x <- score(x)
#' outfile <- file.path(tempdir(), "out.ly")
#' lilypond(x, outfile)
lilypond <- function(score, file, key = "c", time = "4/4", tempo = "2 = 60",
                     header = NULL, paper = NULL, string_names = NULL,
                     endbar = TRUE, midi = TRUE, colors = NULL,
                     crop_png = TRUE, simplify = TRUE){
  if(!is.null(paper$textheight)) crop_png <- FALSE
  crop_png_w <- if(crop_png & !length(header)) TRUE else FALSE
  if(is.null(tempo)){
    if(midi) stop("Set an explicit `tempo` if `midi = TRUE`.", call. = FALSE)
    tempo <- '" "'
  }
  if(!"score" %in% class(score))
    stop("`score` is not a score object.", call. = FALSE)
  major <- ifelse(utils::tail(strsplit(key, "")[[1]], 1) == "m", FALSE, TRUE)
  raw_key <- gsub("m", "", key)
  key <- .notesub(raw_key, simplify = TRUE)
  mode <- ifelse(major, "\\major", "\\minor")
  if((major && !key %in% .keys$major) || (!major && !key %in% .keys$minor))
    stop("Invalid `key`. See `keys()`.", call. = FALSE)
  paper_args <- .lp_paper_args(paper, crop_png, crop_png_w)
  paper <- do.call(.lp_paper, paper_args)
  chords <- attributes(score)$chords
  has_chords <- !is.null(chords)
  if(has_chords) chords <- chords[!names(chords) %in% c("r", "s")]
  chord_seq <- attributes(score)$chord_seq
  has_chord_seq <- !is.null(chord_seq)
  if(has_chords){
    if(!has_chord_seq)
      chord_seq <- stats::setNames(rep(1, length(chords)), names(chords))
    names(chords) <- .notesub(names(chords))
  }
  if(!is.null(chord_seq)) names(chord_seq) <- .notesub(names(chord_seq))
  rel_tp <- ifelse(any(score$ms_transpose != 0), TRUE, FALSE)
  top <- .lp_top(paper_args$fontsize, header, rel_tp, colors, chords)
  colors <- .lp_color_overrides(colors)
  global <- .lp_global(time, key, mode, tempo, endbar, colors)
  cd <- .chord_diagram(chords, chord_seq)
  if(simplify) score$phrase <- lapply(score$phrase, simplify_phrase)
  d <- split(score, score$tabstaff)
  melody0 <- split(score$phrase, score$tabstaff)
  melody_id <- paste0("melody", LETTERS[seq_along(melody0)])
  melody <- paste0(purrr::map_chr(seq_along(d), ~{
    .set_melody(
      melody0[[.x]], d[[.x]], melody_id[.x],
      inject_bg = list(bg = colors$bg, inject = .x == length(d))
    )
  }), collapse = "")
  melody_id_final <- .get_melody_id(melody)

  score <- .set_score(d, melody_id, TRUE, NULL, NULL, tempo, has_chord_seq,
                      string_names, rel_tp, raw_key, colors$score)

  midi_tag <- paste0("  \\midi{\n    \\tempo ", tempo, "\n  }\n", collapse = "")
  midi_melody <- NULL
  if(midi){
    if(any(grepl("\\\\repeat", melody))){
      melody_id2 <- paste0("midi", melody_id)
      midi_melody <- paste0(
        purrr::map_chr(seq_along(d), ~{
          .set_melody(melody0[[.x]], d[[.x]], melody_id2[.x], TRUE)
        }), collapse = "")

      melody_id2_final <- .get_melody_id(midi_melody)
      score2 <- .set_score(d, melody_id, FALSE, midi_tag, melody_id2_final,
                           tempo, FALSE, NULL, rel_tp, raw_key, colors$score)
      melody <- paste0(melody, midi_melody, collapse = "\n\n")
    } else {
      score2 <- .set_score(d, melody_id, FALSE, midi_tag, melody_id_final,
                           tempo, FALSE, NULL, rel_tp, raw_key, colors$score)
    }
    score <- paste0(score, score2, collapse = "\n")
  }
  output <- paste(
    c(.lp_version(), paper, top, global, cd, melody, score), collapse = "")
  write(file = .adjust_file_path(file)$lp, output)
}

.get_melody_id <- function(x){
  x <- strsplit(x, "[ =\n]")[[1]]
  idx <- grep("melody", x)
  x[idx]
}

.adjust_file_path <- function(file){
  file <- gsub("\\\\", "/", file)
  ext <- gsub(".*\\.(.*)$", "\\1", file)
  lily <- gsub(paste0(ext, "$"), "ly", file)
  list(tp = file, lp = lily, ext = ext)
}

.lp_header <- function(title = "", subtitle = "", composer = "", arranger = "",
                       instrument = "", meter = "", opus = "", piece = "",
                       poet = "", copyright = "", tagline = "", color = "",
                       ...){
  x <- list(...)
  if(!is.null(subtitle) & !is.null(x$album)){
    subtitle <- paste(
      if(color == "") "\\markup {" else
        paste0(gsub("markup", "markup {", color), "{"),
      gsub(x$album, paste0("\\\\italic \"", x$album, "\""), subtitle), "}",
      if(color != "") "}"
    )
  } else {
    subtitle <- paste0(color, "\"", subtitle, "\"", collapse = "")
  }
  paste0(
    "\\header {\n", "  title = ", color, "\"", title, "\"\n",
    "  subtitle = ", subtitle, "\n",
    "  composer = ", color, "\"", composer, "\"\n",
    "  arranger = ", color, "\"", arranger, "\"\n",
    "  instrument = ", color, "\"", instrument, "\"\n",
    "  metre = ", color, "\"", meter, "\"\n",
    "  opus = ", color, "\"", opus, "\"\n",
    "  piece = ", color, "\"", piece, "\"\n",
    "  poet = ", color, "\"", poet, "\"\n",
    "  copyright = ", color, "\"", copyright, "\"\n",
    "  tagline = ", color, "\"", tagline, "\"\n}\n")
}

.paper_defaults <- list(textheight = 220, linewidth = 150, indent = 0,
   fontsize = 10, page_numbers = TRUE,
   print_first_page_number = TRUE, first_page_number = 1)

.set_melody <- function(x, d, id, midi = FALSE, rel_tp = FALSE, raw_key,
                        inject_bg = list(bg = "", inject = FALSE)){
  bg <- if(inject_bg$inject) inject_bg$bg else ""
  multivoice <- length(unique(d$voice)) > 1
  if(!multivoice) x0 <- paste0(id, " = {\n  \\global\n  ")
  if(multivoice){
    x <- split(x, d$voice)
    x0 <- paste0(id, LETTERS[as.integer(names(x))], " = {\n  \\global\n  ")
    v <- c("One", "Two")
    x <- purrr::map2(x, seq_along(x), ~{
      paste0("  \\voice", v[.y], " ", paste(.x, collapse = " "), "\n")
    }) %>%
      unlist()
    if(midi) x <- paste("\\unfoldRepeats {", x, "}")
    n <- length(x)
    if(grepl(".*\\}([ \n]+|)$", x[n])){
      x[n] <- gsub("(.*)\\}([ \n]+|)$", "\\1", x[n])
      bg <- paste0("\n", bg, "\n}\n")
    }
    x[n] <- paste0(x[n], bg)
    x <- paste0(x0, "\\override StringNumber #'transparent = ##t\n  ",
                gsub("\n\n", "\n", gsub("\\|", "\\|\n", x)), "}\n\n",
                collapse = "\n")
  } else {
    x <- paste0(paste0(x, collapse = ""), "\n")
    if(midi) x <- paste("\\unfoldRepeats {", x, "}")
    if(grepl(".*\\}([ \n]+|)$", x)){
      x <- gsub("(.*)\\}([ \n]+|)$", "\\1", x)
      bg <- paste0("\n", bg, "\n}\n")
    }
    x <- paste0(x, bg)
    x <- paste0(x0, "\\override StringNumber #'transparent = ##t\n  ",
                gsub("\n\n", "\n", gsub("\\|", "\\|\n", x)), "}\n\n")
  }
  gsub("  ", "", gsub("  ", " ", x))
}

.lp_paper_args <- function(x, crop, cropw){
  y <- list(crop = crop, cropw = cropw)
  if(is.null(x)) return(c(.paper_defaults, y))
  for(i in names(.paper_defaults))
    if(!i %in% names(x)) x[[i]] <- .paper_defaults[[i]]
  c(x, y)
}

.lp_paper <- function(...) {
  x <- list(...)
  crop <- x$crop
  cropw <- x$cropw
  ppn <- x$page_numbers
  if(!is.logical(ppn)) stop("`page_numbers` must be logical.", call. = FALSE)
  pfpn <- x$print_first_page_number
  if(!is.logical(pfpn))
    stop("`Print_first_page_number` must be logical.", call. = FALSE)
  if(!ppn) pfpn <- FALSE
  fpn <- x$first_page_number
  if(!is.numeric(fpn))
    stop("`first_page_number` must be a number.", call. = FALSE)
  set_paper <- paste0(
    "#(set! paper-alist (cons '(\"papersize\" . (cons (* ",
    x$linewidth,
    " mm) (* ",
    x$textheight,
    " mm))) paper-alist))\n"
  )
  paste0(
    if(!crop) set_paper,
    if(crop) "\\paper{\n" else "\\paper{\n  #(set-paper-size \"papersize\")\n",
    if(crop) paste0("  line-width=", x$linewidth, "\\mm\n"),
    if(cropw) "  oddFooterMarkup=##f\n",
    if(cropw) "  oddHeaderMarkup=##f\n",
    if(cropw) "  bookTitleMarkup = ##f\n",
    if(cropw) "  scoreTitleMarkup = ##f\n",
    paste0("  indent = ", x$indent, ".\\mm\n"),
    paste0("  first-page-number = ", fpn, "\n"),
    paste0("  print-page-number = ", ifelse(ppn, "##t", "##f"), "\n"),
    paste0("  print-first-page-number = ", ifelse(pfpn, "##t", "##f"), "\n"),
    "}\n\n"
  )
}

.lp_override_all_colors <- paste0(
  "#(define (override-color-for-all-grobs color)\n",
  " (lambda (context)\n",
  "  (let loop ((x all-grob-descriptions))\n",
  "   (if (not (null? x))\n",
  "    (let ((grob-name (caar x)))\n",
  "     (ly:context-pushpop-property context grob-name 'color color)\n",
  "      (loop (cdr x)))))))\n\n")

.lp_global <- function(time, key, mode, tempo, endbar, colors){
  x <- if(colors$score != "") .lp_override_all_colors else ""
  paste0(x, "global = {\n  \\time ", time, "\n  \\key ", key, " ", mode,
         "\n  \\tempo ", tempo, if(endbar) "\n  \\bar \"|.\"\n",
         colors$overrides, "}\n\n")
}

.lp_top <- function(fontsize, header, rel_tp, colors, chords){
  header <- .header_plus_colors(header, colors)
  if(is.null(header)) header <- list()
  x <- paste0(if(rel_tp) .ly_transpose_defs,
              paste0("#(set-global-staff-size ", fontsize, ")\n"),
              do.call(.lp_header, header),
              "\\include \"predefined-guitar-fretboards.ly\"\n\n")
  if(!is.null(chords)){
    x <- paste0(x, purrr::map_chr(
      seq_along(chords), ~{
        paste0("#(define fb", .x, " (make-fretboard-table))\n",
               "\\storePredefinedDiagram #fb", .x, " \\chordmode{",
               names(chords)[.x], "} #guitar-tuning \"", chords[[.x]], "\"\n")
      }) %>%
        paste(collapse = ""), "\n", collapse = "")
  }
  x
}

.header_plus_colors <- function(header, colors){
  if(!is.null(header) && !is.null(colors) && !is.null(colors$color)){
    header <- c(
      header,
      list(color = paste0(
        "\\markup \\with-color #(rgb-color ",
        paste(round(grDevices::col2rgb(colors$color) / 255, 4),
              collapse = " "), ") "))
    )
  }
  header
}

.chord_diagram <- function(chords, chord_seq){
  if(is.null(chords) & is.null(chord_seq)) return()
  if(!is.null(chords)){
    diagram <- paste0("mychorddiagrams = \\chordmode {\n",
                      paste0("  \\set predefinedDiagramTable = #fb",
                             seq_along(chords), " ",
                             names(chords), "\n", collapse = ""), "}\n\n")
    topcenter <- paste0(
      "\\markup\\vspace #3\n",
      "\\markup \\fill-line {\n  \\score {\n    <<\n      ",
      "\\context ChordNames { \\mychorddiagrams }\n",
      "      \\context FretBoards {\n        ",
      "\\override FretBoards.FretBoard.size = #1.2\n",
      "        \\mychorddiagrams\n      }\n    >>\n  ",
      "\\layout {}\n  }\n}\n\\markup\\vspace #3\n\n")
  } else {
    diagram <- NULL
    topcenter <- NULL
  }
  a <- names(chord_seq)
  modifiers <- purrr::map_chr(strsplit(a, ":"), ~({
    if(length(.x) == 1) NA else .x[2]
  }))
  base_chords <- purrr::map_chr(strsplit(a, ":"), 1)
  alt_bass <- purrr::map_chr(strsplit(base_chords, "/"), ~({
    if(length(.x) == 1) NA else .x[2]
  }))
  base_chords <- purrr::map_chr(strsplit(base_chords, "/"), 1)
  chords <- paste0(base_chords, chord_seq, ifelse(is.na(alt_bass), "",
                                                  paste0("/", alt_bass)),
                   ifelse(is.na(modifiers), "", paste0(":", modifiers)))
  name <- paste0("chordNames = \\chordmode {\n  ",
                 "\\override ChordName.font-size = #2\n  \\global\n  ",
                 paste(chords, collapse = " "), "\n}\n\n")
  paste0(diagram, name, topcenter, collapse = "")
}

.set_score <- function(d, id, layout, midi, midi_melody_id, tempo,
                       has_chord_seq, string_names, rel_tp, raw_key,
                       color){
  if(rel_tp) key <- .notesub(raw_key)
  if(layout){
    clef <- purrr::map_chr(d, ~unique(.x$staff))
    tuning <- purrr::map_chr(d, ~unique(.x$tuning))
    str_lab  <- purrr::map_chr(tuning, .tunelab)
    voice <- purrr::map(d, ~unique(.x$voice))
    ms_tp <- purrr::map_int(d, ~unique(.x$ms_transpose))
    ms_key <- purrr::map_chr(d, ~unique(.x$ms_key))
    show_tab <- purrr::map_lgl(d, ~unique(.x$tab))
    x <- paste0(
      purrr::map_chr(seq_along(clef), ~({
        tp_wrap <- ms_tp[.x] != 0
        if(tp_wrap){
          rel_tp_key <- .notesub(
            transpose(raw_key, ms_tp[.x], "tick", key = ms_key[.x])
          )
          rel_tp_string <- paste("\\transpose", key, rel_tp_key, "{ ")
        }
        multivoice <- length(voice[[.x]]) > 1
        if(!multivoice){
          x2 <- paste0("\\", id[.x])
          x1 <- paste0(if(tp_wrap) rel_tp_string, x2, if(tp_wrap) " }")
        }
        if(multivoice){
          x0 <- paste0(id[.x], LETTERS[voice[[.x]]])
          x1 <- paste0(if(tp_wrap) rel_tp_string, "\\context Voice = \"",
                       x0[1], "\" \\", x0[1], if(tp_wrap) " }",
                       " ", if(tp_wrap) rel_tp_string, "\\context Voice = \"",
                       x0[2], "\" \\", x0[2], if(tp_wrap) " }")
          x2 <- paste0("\\context TabVoice = \"", x0[1], "\" \\", x0[1],
                       " \\context TabVoice = \"", x0[2], "\" \\", x0[2])
        }
        paste0(
          if(!is.na(clef[.x])) paste0("\\new Staff << \\clef \"", clef[.x],
                                      "\" ", x1, " >>\n  ", collapse = ""),
          if(show_tab[.x]){
            paste0("\\new TabStaff \\with { stringTunings = \\stringTuning <",
                   .notesub(tuning[.x]), "> } <<\n    ",
                   if((is.null(string_names) &&
                       tuning[.x] != "e, a, d g b e'") ||
                      (!is.null(string_names) && string_names))
                       paste("\\set TabStaff.instrumentName = \\markup ",
                             "{ \\hspace #7 \\override #'(baseline-skip . 1.5)",
                             " \\column \\fontsize #-4.5 \\sans {",
                             str_lab[.x], "} }\n    "),
                   "\\override Stem #'transparent = ##t\n    ",
                   "\\override Beam #'transparent = ##t\n    ",
                   x2, "\n  >>\n  ", collapse = "")
          }
        )
      })), collapse = "")
  } else {
    ms_tp <- unique(purrr::map_int(d, ~unique(.x$ms_transpose)))
    ms_key <- unique(purrr::map_chr(d, ~unique(.x$ms_key)))
    if(rel_tp && length(ms_tp) == 1 && ms_tp != 0 && length(ms_key) == 1){
      rel_tp_key <- .notesub(transpose(raw_key, ms_tp, "tick", key = ms_key))
      rel_tp_string <- paste("\\transpose", key, rel_tp_key, "{ ")
      midi_melody_id <- paste0(rel_tp_string, "\\", midi_melody_id, " }")
    } else {
      if(rel_tp) warning(
        paste("Multiple music staves with different",
              "transposed key signatures. MIDI output not transposed.")
      )
      midi_melody_id <- paste0("\\", midi_melody_id)
    }
    x <- paste0(paste0(midi_melody_id, collapse = "\n  "), "\n  ")
  }
  paste0("\\score {  <<\n  ", color,
         if(has_chord_seq) "\\new ChordNames \\chordNames\n  ", x, ">>\n",
         if(layout) "  \\layout{ }\n", if(!is.null(midi)) midi, "}")
}

.tunelab <- function(x){
  x <- gsub("[,']", "", x)
  x <- toupper(x)
  x <- gsub("#", "is", x)
  x <- gsub("_", "b", x)
  paste(rev(strsplit(x, " ")[[1]]), collapse = " ")
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
  x <- as.character(
    mapply(
      substr, x, idx, idx + c(diff(idx), nchar(x) - utils::tail(idx, 1) + 1) - 1
    )
  )
  note_tie <- grepl("~", x)
  if(!strings && length(x) > 1 && any(note_tie) && !all(note_tie))
    x[!note_tie] <- paste0(x[!note_tie], "~")
  if(!strings & abb) x <- gsub("aes", "as", gsub("ees", "es", x))
  if(strings && length(idx0))
    x[idx0] <- as.character(as.numeric(x[idx0]) + xdif)
  x
}

.eps_cleanup <- function(file){
  out_dir <- dirname(file)
  file <- basename(file)
  ext <- gsub(".*\\.(.*)$", "\\1", file)
  if(ext == "png"){
    x <- gsub(paste0("\\.", ext), "", file)
    y <- c(".eps", "-1.eps", "-systems.count", "-systems.tex", "-systems.texi")
    x <- paste0(out_dir, "/", x, y)
    unlink(x, recursive = TRUE, force = TRUE)
  }
  invisible()
}


.lp_color_elements <- c("color", "background", "staff", "time", "clef", "bar",
                        "beam", "head", "stem", "accidental", "slur", "tabhead")

.lp_color_overrides <- function(x){
  if(is.null(x)) return(list(overrides = "", score = "", bg = ""))
  idx <- which(!names(x) %in% .lp_color_elements)
  if(length(idx)) x <- x[-idx]
  if(!length(x)) return(list(overrides = "", score = "", bg = ""))
  if("background" %in% names(x)){
    bg <- x$background
    x$background <- NULL
    bg <- paste0(
      "\n -\\tweak layer #-1\n", " -\\markup {",
      " \\with-dimensions #'(0 . 0) #'(0 . 0)",
      " \\with-color #(rgb-color ",
      paste(round(grDevices::col2rgb(bg) / 255, 4), collapse = " "), ")",
      " \\filled-box #'(-1000 . 1000) #'(-1000 . 4000) #0 }\n")
  } else {
    bg <- ""
  }
  if("color" %in% names(x)){
    score <- x$color
    x$color <- NULL
    score <- paste0(
      "\\applyContext #(override-color-for-all-grobs (rgb-color ",
      paste(round(grDevices::col2rgb(score) / 255, 4),
            collapse = " "), "))\n  ")
  } else {
    score <- ""
  }
  if(length(x)){
    id <- purrr::map_chr(names(x), ~{
      switch(
        .x,
        staff = "Staff.StaffSymbol.color",
        time = "Staff.TimeSignature.color",
        clef = "Staff.Clef.color",
        bar = "Staff.BarLine.color",
        beam = "Staff.Beam.color",
        head = "Staff.NoteHead.color",
        stem = "Staff.Stem.color",
        accidental = "Staff.Accidental.color",
        slur = "Staff.Slur.color",
        tabhead = "Staff.TabNoteHead.color"
      )
    })
    x <- purrr::map2_chr(x, id, ~{
      x <- paste0(
        "#(rgb-color ",
        paste(round(grDevices::col2rgb(.x) / 255, 4), collapse = " "), ")")
      paste("  \\override", .y, "=", x)
    }) %>%
      paste(collapse = "\n")
    x <- paste0(x, "\n")
  } else {
    x <- ""
  }
  if(bg != "") x <- paste0(x, "  \\override Staff.TabNoteHead.whiteout = ##f\n")
  list(overrides = x, score = score, bg = bg)
}

.octave_to_tick <- function(x){
  x <- gsub("0", ",,,", x)
  x <- gsub("1", ",,", x)
  x <- gsub("2", ",", x)
  x <- gsub("3", "", x)
  x <- gsub("4", "'", x)
  x <- gsub("5", "''", x)
  x <- gsub("6", "'''", x)
  x <- gsub("7", "''''", x)
  x <- gsub("8", "'''''", x)
  x <- gsub("9", "''''''", x)
  x
}

.octave_to_int <- function(x){
  x <- gsub(",,,,", "_-1_", x)
  x <- gsub(",,,", "0", x)
  x <- gsub(",,", "1", x)
  x <- gsub(",", "2", x)
  x <- gsub("_-1_", ",,,,", x)
  x <- gsub("''''''", "9", x)
  x <- gsub("'''''", "8", x)
  x <- gsub("''''", "7", x)
  x <- gsub("'''", "6", x)
  x <- gsub("''", "5", x)
  x <- gsub("'", "4", x)
  x
}

.notesub <- function(x, simplify = FALSE){
  x <- gsub("#", "is", x)
  x <- gsub("([^-])_", "\\1es", x)
  if(simplify) x <- gsub("ees", "es", gsub("aes", "as", x))
  x
}

.tabsub <- function(x){
  x <- strsplit(x, ";")[[1]]
  x[1] <- gsub("x", "xDEADNOTEx", x[1])
  x[1] <- .notesub(x[1])
  articulated <- grepl("\\[([a-z]+)\\]", x[1])
  if(articulated){
    if(!gsub(".*\\[([a-z]+)\\].*", "\\1", x[1]) %in% tabr::articulations$value)
      stop("Invalid articulation.", call. = FALSE)
  }
  if(articulated) x[1] <- gsub("\\[([a-z]+)\\]", "\\\\\\1", x[1])
  x[1] <- gsub("-([^->\\^_!\\.\\+]|$)", "\\\\glissando", x[1])
  x[1] <- gsub("-\\\\glissando", "--", x[1])
  if(length(x) == 2){
    x[2] <- paste0(";", substr(x[2], 1, 1), gsub("_", " ", substring(x[2], 2)))
    x <- paste0(x, collapse = "")
  }
  x
}

.strsub <- function(x){
  if(any(is.na(x))) x[is.na(x)] <- "x"
  f <- function(x){
    strsplit(gsub("\\(", " \\(", gsub("\\)", " ", x)), " ")[[1]] %>%
      purrr::map(~({
        if(substr(.x, 1, 1) == "(") substring(.x, 2) else strsplit(.x, "")[[1]]
      })) %>%
      unlist() %>%
      paste0(collapse = "_")
  }
  purrr::map_chr(x, f)
}

.noterev <- function(x){
  purrr::map(strsplit(x, " ")[[1]], ~({
    x <- gsub("as", "aes", .x)
    x <- gsub("^es| es", "ees", x)
    x <- .split_chord(x)
    x <- gsub("is", "#", x)
    x <- gsub("^as|^aes", "a_", x)
    x <- gsub("^ees|^es", "e_", x)
    x <- gsub("es", "_", x)
    paste(x, collapse = "")
  })) %>%
    paste(collapse = " ")
}

.keys <- list(
  major = .notesub(dplyr::filter(.keydata, major)$key, simplify = TRUE),
  minor = .notesub(gsub("m", "", dplyr::filter(.keydata, !major)$key),
                   simplify = TRUE)
)
