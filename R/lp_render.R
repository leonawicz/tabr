#' Render sheet music with LilyPond
#'
#' Render sheet music/tablature from a music score with LilyPond.
#'
#' Generate a pdf or png of a music score using the LilyPond music engraving
#' program.
#' Output format is inferred from `file` extension. This function is a wrapper
#' around [lilypond()], the function that creates the LilyPond (`.ly`) file.
#'
#' `render_score()` renders `score()` to pdf or png. `render_midi()` renders a
#' MIDI file based on `score()`. This is still done via LilyPond. The sheet
#' music is created automatically in the process behind the scenes but is
#' deleted and only the MIDI output is retained.
#'
#' `tab()` or `render_tab()` (equivalent) produces both the sheet music and the
#' MIDI file output by default and includes other arguments such as the
#' tablature-relevant argument `string_names`. This is the all-purpose function.
#' Also use this when you intend to create both a sheet music document and a
#' MIDI file.
#'
#' Remember that whether a track contains a tablature staff, standard music
#' staff, or both, is defined in each individual track object contained in
#' `score()`. It is the contents you have assembled in`score()` that dictate
#' what render function you should use. `render_tab()` is general and always
#' works, but `render_score()` would not be the best choice when a tablature
#' staff is present unless you accept the default string naming convention.
#'
#' `render_midi()` is different from `midily()` and `miditab()`, whose purpose
#' is to create sheet music from an existing MIDI file using a LilyPond command
#' line utility.
#'
#' For Windows users, add the path to the LilyPond executable to the system
#' path variable. For example, if the file is at
#'  `C:/lilypond-2.24.2/bin/lilypond.exe`, then add `C:/lilypond-2.24.2/bin` to
#'  the system path.
#'
#' @param score a score object.
#' @param file character, output file ending in .pdf or .png for sheet music or
#' tablature for `score()`. May include an absolute or relative path. For
#' `render_midi()`, a file ending in `.mid`.
#' @param key character, key signature, e.g., `c`, `b_`, `f#m`, etc.
#' @param time character, defaults to `"4/4"`.
#' @param tempo character, defaults to `"2 = 60"`. Set to `NULL` to suppress
#' display of the time signature in the output.
#' @param header a named list of arguments passed to the header of the LilyPond
#' file. See `lilypond()` for details.
#' @param paper a named list of arguments for the LilyPond file page layout. See
#' `lilypond()` for details.
#' @param string_names label strings at beginning of tab staff. `NULL` (default)
#' for non-standard tunings only, `TRUE` or `FALSE` for force on or off
#' completely.
#' @param endbar character, the global end bar.
#' @param midi logical, output midi file in addition to sheet music.
#' @param colors a named list of LilyPond element color overrides. See
#' `lilypond()` for details.
#' @param crop_png logical, see `lilypond()` for details.
#' @param transparent logical, transparent background, png only.
#' @param res numeric, resolution, png only. `transparent = TRUE` may fail when
#' `res` exceeds ~150.
#' @param keep_ly logical, keep the intermediary LilyPond file.
#' @param simplify logical, uses `simplify_phrase()` to convert to simpler,
#' more efficient LilyPond syntax for the LilyPond file before rendering it.
#' @param details logical, set to `TRUE` to print LilyPond log output to
#' console. Windows only.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso [lilypond()], [render_chordchart()],
#' [miditab()]
#'
#' @examples
#' if(tabr_options()$lilypond != ""){
#'   x <- phrase("c ec'g' ec'g'", "4 4 2", "5 432 432")
#'   x <- track(x)
#'   x <- score(x)
#'   outfile <- file.path(tempdir(), "out.pdf")
#'   tab(x, outfile) # requires LilyPond installation
#' }
tab <- function(score, file, key = "c", time = "4/4", tempo = "2 = 60",
                header = NULL, paper = NULL, string_names = NULL, endbar = "|.",
                midi = TRUE, colors = NULL, crop_png = TRUE,
                transparent = FALSE, res = 150, keep_ly = FALSE,
                simplify = TRUE, details = FALSE){
  .check_lilypond()
  fp <- .adjust_file_path(file)
  ext <- if(fp$ext == "pdf") "--pdf" else paste0("-dresolution=", res, " --png")
  if(is.null(paper$textheight) & fp$ext == "png"){
    png_args <- "\" -dbackend=eps "
  } else {
    png_args <-  "\" "
  }
  if(transparent & fp$ext == "png")
    png_args <- paste0(png_args, "\ -dpixmap-format=pngalpha ")
  if(fp$ext == "pdf") crop_png <- FALSE
  if(details) cat("#### Engraving score to", fp$tp, "####\n")
  lilypond(score, fp$lp, key, time, tempo, header, paper,
           string_names, endbar, midi, colors, crop_png, simplify)

  lp_path <- tabr_options()$lilypond
  is_windows <- Sys.info()[["sysname"]] == "Windows"
  if(lp_path == ""){
    lp_path <- if(is_windows) "lilypond.exe" else "lilypond"
  }
  call_string <- paste0("\"", lp_path, png_args, ext,
                        " -dstrip-output-dir=#f \"", fp$lp, "\"")
  if(is_windows){
    system(call_string, show.output.on.console = details)
  } else {
    system(call_string)
  }
  if(!keep_ly) unlink(fp$lp)

  if(crop_png){
    file_cropped <- gsub(".ly$", ".cropped.png", .adjust_file_path(file)$lp)
    if(file.exists(file_cropped)){
      file_keep <- gsub(".cropped.png$", ".png", file_cropped)
      unlink(file_keep, recursive = TRUE, force = TRUE)
      file.copy(file_cropped, file_keep, overwrite = TRUE)
      unlink(file_cropped, recursive = TRUE, force = TRUE)
    }
  }

  .eps_cleanup(fp$tp)
}

#' @export
#' @rdname tab
render_tab <- tab

#' @export
#' @rdname tab
render_score <- function(score, file, key = "c", time = "4/4", tempo = "2 = 60",
                         header = NULL, paper = NULL, endbar = "|.",
                         colors = NULL, crop_png = TRUE, transparent = FALSE,
                         res = 150, keep_ly = FALSE, simplify = TRUE,
                         details = FALSE){
  tab(score, file, key, time, tempo, header, paper, FALSE, endbar, FALSE,
      colors, crop_png, transparent, res, keep_ly, simplify, details)
}

#' @export
#' @rdname tab
render_midi <- function(score, file, key = "c", time = "4/4", tempo = "2 = 60"){
  file0 <- gsub("\\.mid$", ".png", file)
  tab(score, file0, key, tempo = tempo, midi = TRUE)
  unlink(file0, recursive = TRUE, force = TRUE)
  invisible()
}

#' Render a chord chart with LilyPond
#'
#' Render a standalone chord chart of chord fretboard diagrams with LilyPond
#' for a set of chords.
#'
#' This function uses a generates a LilyPond template for displaying only a
#' fretboard diagram chart. It then passes the file to LilyPond for rendering.
#' To plot specific fretboard diagrams in R using ggplot and with greater
#' control, use `plot_fretboard()`.
#'
#' The options for `paper` include the following and have the following
#' default values if not provided.
#'
#' * `textheight = 220`
#' * `linewidth = 150`
#' * `indent = 0`
#' * `fontsize = 10`
#' * `page_numbers = FALSE`
#' * `print_first_page_number = TRUE`
#' * `first_page_number = 1`
#'
#' `fontsize` only controls the global font size. If you want to scale the
#' size of the fretboard diagrams up or down use the the `size` argument
#' rather than this `paper` value.
#'
#' Note that chord chart output must fit on a single page. If the full set of
#' chord diagrams does not fit on one page then diagrams will be clipped in the
#' rendered output. Use `size` to keep the output to one page or make
#' multiple sheets separately.
#'
#' @param chords named character vector of valid formatting for LilyPond chord
#' names and values. See examples.
#' @param file output file.
#' @param size numeric, size of fretboard diagrams (relative to paper font
#' size). Use this to scale diagrams up or down.
#' @param header a named list of arguments passed to the header of the
#' LilyPond file. See details.
#' @param paper a named list of arguments for the LilyPond file page layout.
#' See details.
#' @param colors reserved; not yet implemented for this function.
#' @param crop_png logical, see `lilypond()` for details.
#' @param transparent logical, transparent background, png only.
#' @param res numeric, resolution, png only. `transparent = TRUE` may fail when
#' `res` exceeds ~150.
#' @param keep_ly logical, keep intermediate LilyPond file.
#' @param details logical, set to `TRUE` to print LilyPond log output to
#' console. Windows only.
#'
#' @return writes files to disk
#' @export
#' @seealso [plot_fretboard()], [lilypond()],
#' [tab()]
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#'
#' chords <- filter(
#'   guitarChords, root %in% c("c", "f") & id %in% c("7", "M7", "m7") &
#'   !grepl("#", notes) & root_fret <= 12) |>
#'   arrange(root, id)
#' chords <- setNames(chords$fretboard, chords$lp_name)
#' head(chords)
#'
#' # requires LilyPond installation
#' if(tabr_options()$lilypond != ""){
#'   outfile <- file.path(tempdir(), "out.pdf")
#'   hdr <- list(
#'     title = "Dominant 7th, major 7th and minor 7th chords",
#'     subtitle = "C and F root"
#'   )
#'   render_chordchart(chords, outfile, 2, hdr, list(textheight = 175))
#' }
render_chordchart <- function(chords, file, size = 1.2, header = NULL,
                              paper = NULL, colors = NULL, crop_png = TRUE,
                              transparent = FALSE, res = 150, keep_ly = FALSE,
                              details = FALSE){
  .check_lilypond()
  header <- .header_plus_colors(header, colors)
  colors <- .lp_color_overrides(colors)
  if(colors$score != ""){
    global <- paste0(.lp_override_all_colors, "global = {\n  ",
                     colors$overrides, "}\n\n")
  } else {
    global <- ""
  }
  i <- seq_along(chords)
  id <- names(chords)
  fp <- .adjust_file_path(file)
  ext <- if(fp$ext == "pdf") "--pdf" else paste0("-dresolution=", res, " --png")
  if(is.null(paper$textheight) & fp$ext == "png"){
    png_args <- "\" -dbackend=eps "
  } else {
    png_args <-  "\" "
  }
  if(transparent & fp$ext == "png")
    png_args <- paste0(png_args, "\ -dpixmap-format=pngalpha ")
  crop_png_w <- if(crop_png & !length(header)) TRUE else FALSE
  paper_args <- .lp_paper_args2(paper, crop_png, crop_png_w)
  paper <- do.call(.lp_paper, paper_args)
  x <- paste0(
    "#(set-global-staff-size ", paper_args$fontsize,
    ")\n", do.call(.lp_header, if(is.null(header)) list() else header), "\n",
    global, "\\include \"predefined-guitar-fretboards.ly\"\n"
  )
  def <- purrr::map_chr(i, ~.define_chord(.x, id[.x], chords[.x])) |>
    paste(collapse = "")
  x <- paste0(x, "\n", def, "\nmychorddiagrams = \\chordmode {\n")
  set <- purrr::map_chr(i, ~.set_chord(.x, id[.x])) |> paste(collapse = "")
  x <- paste0(x, set, "}\n\nchordNames = \\chordmode {\n",
              "  \\override ChordName.font-size = #2\n  ",
              paste(names(chords), collapse = " "), "\n}\n\n")
  markup <- paste0(
    "\\markup\\vspace #3\n", "\\markup \\fill-line {\n", "  \\score {\n",
    "    <<\n", "      \\context ChordNames { \\mychorddiagrams }\n",
    "      \\context FretBoards {\n",
    "        \\override FretBoards.FretBoard.size = #", size, "\n",
    "        \\mychorddiagrams\n", "      }\n", "    >>\n", "  \\layout {}\n",
    "  }\n}\n\\markup\\vspace #3\n")
  x <- paste0(.lp_version(), paper, x, markup)
  write(file = fp$lp, x)

  lp_path <- tabr_options()$lilypond
  is_windows <- Sys.info()[["sysname"]] == "Windows"
  if(lp_path == ""){
    lp_path <- if(is_windows) "lilypond.exe" else "lilypond"
  }
  call_string <- paste0("\"", lp_path, png_args, ext,
                        " -dstrip-output-dir=#f \"", fp$lp, "\"")
  if(is_windows){
    system(call_string, show.output.on.console = details)
  } else {
    system(call_string)
  }
  if(!keep_ly) unlink(fp$lp)
  .eps_cleanup(file)
}

.paper_defaults2 <- list(textheight = 220, linewidth = 150, indent = 0,
                         fontsize = 14, page_numbers = FALSE,
                         print_first_page_number = TRUE, first_page_number = 1)

.lp_paper_args2 <- function(x, crop, cropw){
  y <- list(crop = crop, cropw = cropw)
  if(is.null(x)) return(c(.paper_defaults2, y))
  for(i in names(.paper_defaults2))
    if(!i %in% names(x)) x[[i]] <- .paper_defaults2[[i]]
    c(x, list(crop = crop, cropw = cropw))
}

.define_chord <- function(i, id, value){
  paste0("#(define fb", i, " (make-fretboard-table))\n",
         "\\storePredefinedDiagram #fb", i,
         " \\chordmode{", id, "} #guitar-tuning \"", value, "\"\n")
}

.set_chord <- function(i, id){
  paste0("  \\set predefinedDiagramTable = #fb", i, " ", id, "\n")
}
