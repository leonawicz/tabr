#' Save score to LilyPond file
#'
#' Write a score to a LilyPond format (`.ly`) text file for later use by
#' LilyPond or subsequent editing outside of R.
#'
#' @details
#' This function only writes a LilyPond file to disk. It does not require a
#' LilyPond installation. It checks for the version number of an installation,
#' but LilyPond is not required to be found.
#'
#' This function can be used directly but is commonly used by `render_*`
#' functions, which call this function internally to create the LilyPond file
#' and then call LilyPond to render that file to sheet music.
#'
#' @section Header options:
#' All `header` list elements are character strings. The options for `header`
#' include the following.
#'
#' * `title`
#' * `subtitle`
#' * `composer`
#' * `album`
#' * `arranger`
#' * `instrument`
#' * `meter`
#' * `opus`
#' * `piece`
#' * `poet`
#' * `copyright`
#' * `tagline`
#'
#' @section Paper options:
#' All `paper` list elements are numeric except `page_numbers` and
#' `print_first_page_number`, which are logical. `page_numbers = FALSE`
#' suppresses all page numbering. When `page_numbers = TRUE`, you can set
#' `print_first_page_number = FALSE` to suppress printing of only the first page
#' number. `first_page_number` is the number of the first page, defaulting to 1,
#' and determines all subsequent page numbers. These arguments correspond to
#' LilyPond paper block variables.
#'
#' The options for `paper` include the following and have the following default
#' values if not provided.
#'
#' * `textheight = 220`
#' * `linewidth = 150`
#' * `indent = 0`
#' * `fontsize = 10`
#' * `page_numbers = TRUE`
#' * `print_first_page_number = TRUE`
#' * `first_page_number = 1`
#'
#' @section PNG-related options:
#' By default `crop_png = TRUE`. This alters the template so that when
#' the LilyPond output file is created, it contains specifications for cropping
#' the image to the content when that file is rendered by LilyPond to png.
#' The image will have its width and height automatically cropped rather than
#' retain the standard page dimensions. This only applies to png outputs made
#' from the LilyPond file, not pdf. The argument is also ignored if explicitly
#' providing `textheight` to `paper`. You may still provide `linewidth` to
#' `paper` if you find you need to increase it beyond the default 150mm,
#' generally as a result of using a large `fontsize`. Various `render_*`
#' functions that wrap `lilypond` make use of this argument as well.
#'
#' @section Color options:
#' You can provide a named list of global color overrides for various sheet
#' music elements with the `colors` argument of `lilypond` or one of the
#' associated rendering functions.
#'
#' By default, everything is black. Overrides are only inserted into the
#' generated LilyPond file if given. Values are character; either the hex color
#' or a named R color. The named list options include the following.
#'
#' * `color`
#' * `background`
#' * `staff`
#' * `time`
#' * `key`
#' * `clef`
#' * `bar`
#' * `beam`
#' * `head`
#' * `stem`
#' * `accidental`
#' * `slur`
#' * `tabhead`
#' * `lyrics`
#'
#' `color` is a global font color for the entire score. It affects staff
#' elements and `header` elements. It does not affect everything, e.g., page
#' numbers. `background` controls the background color of the entire page. Do
#' not use this if making a transparent background png with the `transparent`
#' argument available in the various `render_*` functions. The other options are
#' also global but override `color`. You can change the color of elements
#' broadly with `color` and then change the color of specific elements using the
#' other options.
#'
#' There are currently some limitations. Specifically, if you provide any
#' `background` color override, most `header` elements will not display.
#'
#' @param score a score object.
#' @param file character, LilyPond output file ending in `.ly`. May include an
#' absolute or relative path.
#' @param key character, key signature, e.g., `c`, `b_`, `f#m`, etc.
#' @param time character, defaults to `"4/4"`.
#' @param tempo character, defaults to `"2 = 60"`. Set to `NA` or `NULL` to
#' suppress metronome mark in output. If suppressed and `midi = TRUE`, an error
#' is thrown.
#' @param header a named list of arguments passed to the header of the LilyPond
#' file. See details.
#' @param paper a named list of arguments for the LilyPond file page layout.
#' See details.
#' @param string_names label strings at beginning of tab staff. `NULL` (default)
#' for non-standard tunings only, `TRUE` or `FALSE` for force on or off
#' completely.
#' @param endbar character, the global end bar.
#' @param midi logical, add midi inclusion specification to LilyPond file.
#' @param colors a named list of LilyPond element color overrides. See details.
#' @param crop_png logical, alter template for cropped height. See
#' details.
#' @param simplify logical, uses `simplify_phrase()` to convert to simpler,
#' more efficient LilyPond syntax.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso [tab()], [render_chordchart()],
#' [midily()]
#'
#' @examples
#' \dontrun{
#' x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#' x <- track(x)
#' x <- score(x)
#' outfile <- file.path(tempdir(), "out.ly")
#' lilypond(x, outfile, midi = FALSE)
#' }
lilypond <- function(score, file, key = "c", time = "4/4", tempo = "2 = 60",
                     header = NULL, paper = NULL, string_names = NULL,
                     endbar = "|.", midi = TRUE, colors = NULL,
                     crop_png = TRUE, simplify = TRUE){
  if(!is.null(paper$textheight)) crop_png <- FALSE
  crop_png_w <- if(crop_png & !length(header)) TRUE else FALSE
  if(is.null(tempo) || is.na(tempo)){
    if(midi) stop("Set an explicit `tempo` if `midi = TRUE`.", call. = FALSE)
    tempo <- '" "'
  }
  if(!inherits(score, "score"))
    stop("`score` is not a score object.", call. = FALSE)
  score <- dplyr::arrange(score, .data[["id"]], .data[["voice"]])
  score$tuning <- .lp_score_tuning(score$tuning)
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
  top <- .lp_top(paper_args$fontsize, header, colors, chords)
  colors <- .lp_color_overrides(colors)
  global <- .lp_global(time, .lp_key_string(key), tempo, endbar, colors)
  cd <- .chord_diagram(chords, chord_seq)
  if(simplify) score$phrase <- lapply(score$phrase, simplify_phrase)
  d <- split(score, score$id)
  melody0 <- split(score$phrase, score$id)
  melody_id <- paste0("melody", LETTERS[seq_along(melody0)])
  melody <- paste0(purrr::map_chr(seq_along(d), ~{
    .set_melody(
      melody0[[.x]], d[[.x]], melody_id[.x],
      inject_bg = list(bg = colors$bg, inject = .x == length(d))
    )
  }), collapse = "")
  melody_id_final <- .get_melody_id(melody)

  score <- .set_score(d, melody_id, TRUE, NULL, NULL, tempo, has_chord_seq,
                      string_names, key, colors$score)

  midi_tag <- paste0(
    "  \\midi{\n    \\tempo ", tempo, "\n  }\n", collapse = ""
  )

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
                           tempo, FALSE, NULL, key, colors$score)
      melody <- paste0(melody, midi_melody, collapse = "\n\n")
    } else {
      score2 <- .set_score(d, melody_id, FALSE, midi_tag, melody_id_final,
                           tempo, FALSE, NULL, key, colors$score)
    }
    score <- paste0(c(score, score2), collapse = "\n")
  }
  global_crop <- if(crop_png_w) "#(ly:set-option 'crop #t)" else NULL
  output <- paste(
    c(.lp_version(), global_crop, paper, top, global, cd, melody, score), collapse = "")
  write(file = .adjust_file_path(file)$lp, output)
}

.lp_key_string <- function(key){
  if(is.na(key)) return("")
  major <- ifelse(utils::tail(strsplit(key, "")[[1]], 1) == "m", FALSE, TRUE)
  x <- .notesub(gsub("m", "", key), simplify = TRUE)
  y <- paste0("\\key ", x, " \\", ifelse(major, "major ", "minor "))
  if((major && !x %in% .keys$major) || (!major && !x %in% .keys$minor))
    stop("Invalid `key`. See `keys()`.", call. = FALSE)
  y
}

.lp_score_tuning <- function(x){
  sapply(x, function(x){
    paste(.notesub(.split_chords(x), simplify = TRUE), collapse = " ")
  })
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

.set_melody <- function(x, d, id, midi = FALSE,
                        inject_bg = list(bg = "", inject = FALSE)){
  bg <- if(inject_bg$inject) inject_bg$bg else ""
  multivoice <- length(unique(d$voice)) > 1
  if(!multivoice) x0 <- paste0(id, " = {\n  \\global\n  ")
  if(multivoice){
    x <- split(x, d$voice)
    x0 <- paste0(id, LETTERS[as.integer(names(x))], " = {\n  \\global\n  ")
    v <- c("One", "Two")
    x <- purrr::map2(x, seq_along(x), ~{
      paste0("\\voice", v[.y], " ", paste(.x, collapse = " "), "\n")
    }) |>
      unlist()
    if(midi) x <- paste("\\unfoldRepeats {", x, "}")
    x <- paste0(x0, "\\override StringNumber.transparent = ##t\n  ",
                gsub("\n\n", "\n", x), "}\n\n",
                collapse = "\n")

    lyrics <- split(d$lyrics, d$voice)
    lyrics <- purrr::map(seq_along(lyrics), ~{
      x <- lyrics[[.x]]
      if(is.na(x)) return(NA)
      x <- strsplit(x, " ")[[1]]
      idx <- grep("\\d", x)
      if(length(idx)) x[idx] <- paste0("\"", x[idx], "\"")
      x <- paste(x, collapse = " ")
      x <- gsub("\\.", "\\\\skip 1", x)
      paste0(gsub("melody", "lyrics", id), LETTERS[.x], " = \\lyricmode {\n  ",
             x, "\n}\n")
    }) |>
      unlist()
    lyrics <- lyrics[!is.na(lyrics)]
    if(length(lyrics) > 1) lyrics <- paste(lyrics, collapse = "\n")
    if(length(lyrics)) x <- paste0(x, lyrics, "\n")
  } else {
    x <- paste0(paste0(x, collapse = ""), "\n")
    if(midi) x <- paste("\\unfoldRepeats {", x, "}")
    if(grepl(".*\\}([ \n]+|)$", x)){
      x <- gsub("(.*)\\}([ \n]+|)$", "\\1", x)
      bg <- paste0("\n", bg, "\n}\n")
    }
    x <- paste0(x, bg)
    x <- paste0(x0, "\\override StringNumber.transparent = ##t\n  ",
                gsub("\n\n", "\n", x), "}\n\n")
    if(!is.na(d$lyrics)){
      lyrics <- strsplit(d$lyrics, " ")[[1]]
      idx <- grep("\\d", lyrics)
      if(length(idx)) lyrics[idx] <- paste0("\"", lyrics[idx], "\"")
      lyrics <- paste(lyrics, collapse = " ")
      x <- paste0(x, gsub("melody", "lyrics", id), " = \\lyricmode {\n  ",
                  gsub("\\.", "\\\\skip 1", lyrics), "\n}\n\n")
    }
  }
  x <- gsub(" \n\\}\n\\}", " }\n}", x)
  x <- gsub("  ", " ", x)
  x <- gsub("  ", "", x)
  x <- gsub("\n\n\n", "\n\n", x)
  x
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
    stop("`print_first_page_number` must be logical.", call. = FALSE)
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
    if(cropw) "  oddFooterMarkup = ##f\n",
    if(cropw) "  oddHeaderMarkup = ##f\n",
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

.lp_global <- function(time, key, tempo, endbar, colors){
  x <- if(colors$score != "") .lp_override_all_colors else ""
  paste0(
    x, "global = {\n  \\time ", time,
    if(tempo != '" "') paste0("\n  \\tempo ", tempo),
    "\n  \\bar \"", endbar, "\"\n", colors$overrides, "}\n\n",
    "global_key = {\n ", key, "\n}\n\n"
  )
}

.lp_top <- function(fontsize, header, colors, chords){
  header <- .header_plus_colors(header, colors)
  if(is.null(header)) header <- list()
  x <- paste0("#(set-global-staff-size ", fontsize, ")\n",
              do.call(.lp_header, header),
              "\\include \"predefined-guitar-fretboards.ly\"\n\n")
  if(!is.null(chords)){
    x <- paste0(x, purrr::map_chr(
      seq_along(chords), ~{
        paste0("#(define fb", .x, " (make-fretboard-table))\n",
               "\\storePredefinedDiagram #fb", .x, " \\chordmode{",
               names(chords)[.x], "} #guitar-tuning \"", chords[[.x]], "\"\n")
      }) |>
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
                       has_chord_seq, string_names, key, color){
  if(layout){
    clef <- purrr::map_chr(d, ~unique(.x$clef))
    tuning <- purrr::map_chr(d, ~unique(.x$tuning))
    str_lab  <- purrr::map_chr(tuning, .tunelab)
    voice <- purrr::map(d, ~unique(.x$voice))
    track_key <- purrr::map_chr(d, ~unique(.x$key))
    show_tab <- purrr::map_lgl(d, ~unique(.x$tab))
    lyrics <- purrr::map(d, ~unique(.x$lyrics))
    x <- paste0(
      purrr::map_chr(seq_along(clef), ~({
        tkey <- .lp_key_string(track_key[.x])
        if(tkey == "") tkey <- "\\global_key "
        multivoice <- length(voice[[.x]]) > 1
        if(multivoice){
          x0 <- paste0(id[.x], LETTERS[voice[[.x]]])
          x1 <- paste0("\\context Voice = \"", x0[1], "\" \\", x0[1],
                       " ", "\\context Voice = \"", x0[2], "\" \\", x0[2])
          x2 <- gsub("Voice ", "TabVoice ", x1)
          lyrics1 <- .set_score_lyrics(lyrics[[.x]][1], x0[1])
          lyrics2 <- .set_score_lyrics(lyrics[[.x]][2], x0[2])
          lyrics <- paste0(lyrics1, lyrics2)
        } else {
          x1 <- x2 <- paste0("\\", id[.x])
          lyrics <- .set_score_lyrics(lyrics[[.x]], id[.x])
        }
        if(lyrics != "" & !multivoice)
          x1 <- paste0("\\context Voice = \"", gsub("\\\\", "", x1), "\" ", x1)
        paste0(
          if(!is.na(clef[.x]))
            paste0("\\new Staff << \\clef \"", clef[.x], "\" ", tkey, x1,
                   " >>\n  ", collapse = ""),
          lyrics,
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
                   "\\override Stem.transparent = ##t\n    ",
                   "\\override Beam.transparent = ##t\n    ",
                   x2, "\n  >>\n  ", collapse = "")
          }
        )
      })), collapse = "")
  } else {
    x <- paste0(paste0("\\", midi_melody_id, collapse = "\n  "), "\n  ")
  }
  paste0("\\score {  <<\n  ", color,
         if(has_chord_seq) "\\new ChordNames \\chordNames\n  ", x, ">>\n",
         if(layout) "  \\layout{ }\n", if(!is.null(midi)) midi, "}\n")
}

.set_score_lyrics <- function(lyrics, melody){
  if(is.na(lyrics)){
    ""
  } else {
    paste0("\\new Lyrics \\lyricsto \"", melody, "\" { \\",
           gsub("melody", "lyrics", melody), " }\n  ")
  }
}

.tunelab <- function(x){
  x <- gsub("[,']", "", x)
  x <- toupper(x)
  x <- strsplit(x, " ")[[1]]
  x <- gsub("(.*)(IS)", "\"\\1#\"", x)
  x <- gsub("^(A|E)S$", "\\1b", x)
  x <- gsub("ES", "b", x)
  paste(rev(x), collapse = " ")
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


.lp_color_elements <- c("color", "background", "staff", "time", "key", "clef", "bar",
                        "beam", "head", "stem", "accidental", "slur", "tabhead",
                        "lyrics")

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
        key = "Staff.KeySignature.color",
        clef = "Staff.Clef.color",
        bar = "Staff.BarLine.color",
        beam = "Staff.Beam.color",
        head = "Staff.NoteHead.color",
        stem = "Staff.Stem.color",
        accidental = "Staff.Accidental.color",
        slur = "Staff.Slur.color",
        tabhead = "Staff.TabNoteHead.color",
        lyrics = "Lyrics.LyricText.color"
      )
    })
    x <- purrr::map2_chr(x, id, ~{
      x <- paste0(
        "#(rgb-color ",
        paste(round(grDevices::col2rgb(.x) / 255, 4), collapse = " "), ")")
      paste("  \\override", .y, "=", x)
    }) |>
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
  articulated <- grepl("\\[(.*)\\]", x[1])
  if(articulated){
    if(!gsub(".*\\[(.*)\\].*", "\\1", x[1]) %in% tabr::articulations$value)
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
    strsplit(gsub("\\(", " \\(", gsub("\\)", " ", x)), " ")[[1]] |>
      purrr::map(~({
        if(substr(.x, 1, 1) == "(") substring(.x, 2) else strsplit(.x, "")[[1]]
      })) |>
      unlist() |>
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
  })) |>
    paste(collapse = " ")
}

.keys <- list(
  major = .notesub(dplyr::filter(.keydata, major)$key, simplify = TRUE),
  minor = .notesub(gsub("m", "", dplyr::filter(.keydata, !major)$key),
                   simplify = TRUE)
)
