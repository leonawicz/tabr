#' Render sheet music snippet with LilyPond
#'
#' Render a sheet music/tablature snippet from a music object with LilyPond.
#'
#' These functions allow you to render short, simple snippets of sheet music
#' directly from a \code{music} object. This is useful when you do not need to
#' build up from phrases to tracks to a full score. They treat \code{music} as
#' a single voice for a single track. This simplifies the possible output but
#' is very convenient when this is all you need.
#'
#' These functions abstract the following pipeline,
#'
#' \code{music \%>\% phrase() \%>\% track() \%>\% score() \%>\% render_*()}
#'
#' for this simple edge case and directly expose the most relevant arguments.
#'
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
#'   \item \code{fontsize = 20}
#'   \item \code{page_numbers = FALSE}
#'   \item \code{print_first_page_number = TRUE}
#'   \item \code{first_page_number = 1}
#' }
#'
#' \code{textheight = 150} is the default, but for music snippet rendering,
#' a value must be provided explicitly via \code{paper} when rendering to png.
#' Otherwise for png outputs the height is cropped automatically rather than
#' remaining a full page. See \code{lilypond} for details.
#'
#' Passing arguments to \code{header} can completely or partially prevent
#' cropping in both direction, which must then be done manually with
#' \code{linewidth} and \code{textheight}. This is all based on underlying
#' LilyPond behavior.
#'
#' If \code{music} contains lyrics and there are rests in the note sequence,
#' note-lyric alignment is maintained automatically when these functions
#' remove the lyric timesteps corresponding to the rests prior to sending to
#' LilyPond. LilyPond skips rests when engraving lyrics and expects a shortened
#' lyrics sequence in comparison to how \code{tabr} matches by timestep
#' including rests. This is in contrast to \code{track}, for which you have to
#' shorten the lyrics object yourself prior to combining with a phrase object
#' that has rests.
#'
#' @param music a music object.
#' @param file character, output file ending in .pdf or .png.
#' @param clef character, include a music staff with the given clef.
#' \code{NA} to suppress. See \code{track} for details.
#' @param tab logical, include tablature staff. \code{NA} to suppress. See
#' \code{track}.
#' @param tuning character, string tuning, only applies to tablature. See
#' \code{track}.
#' @param string_names label strings at beginning of tab staff. \code{NULL}
#' (default) for non-standard tunings only, \code{TRUE} or \code{FALSE} for
#' force on or off completely.
#' @param header a named list of arguments passed to the header of the
#' LilyPond file. See \code{lilypond} details.
#' @param paper a named list of arguments for the LilyPond file page layout.
#' See \code{lilypond} details.
#' @param midi logical, also output an corresponding MIDI file.
#' @param colors a named list of LilyPond element color global overrides. See
#' \code{lilypond} for details.
#' @param transparent logical, transparent background, png only.
#' @param res numeric, resolution, png only. \code{transparent = TRUE} may fail
#' when \code{res} exceeds ~150.
#' @param keep_ly logical, keep the intermediary LilyPond file.
#' @param simplify logical, uses \code{simplify_phrase} to convert to simpler,
#' more efficient LilyPond syntax.
#'
#' @return nothing returned; a file is written.
#' @export
#' @seealso \code{\link{plot_music}}, \code{\link{phrase}}, \code{\link{track}},
#' \code{\link{score}}, \code{\link{lilypond}}, \code{\link{tab}}
#'
#' @examples
#' x <- "a,4;5*5 b,- c cgc'e'~ cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
#' x <- as_music(x)
#'
#' y <- "a,,4;3*5 b,,- c, c,g,c~ c,g,c1 c4;1 g,;2 c,;3 g,;2 c,c1;31"
#' y <- as_music(y)
#'
#' z <- as_music("a,4 b, r c~ c2 d", lyrics = as_lyrics("A2 B2 . C3 . D3"))
#'
#' \dontrun{
#' if(tabr_options()$lilypond != ""){ # requires LilyPond installation
#'   outfile <- file.path(tempdir(), "out.pdf")
#'   render_music(x, outfile)
#'
#'   outfile <- file.path(tempdir(), "out.png")
#'   render_music(x, outfile, "treble_8", tab = TRUE)
#'
#'   render_music_tc(x, outfile)
#'   render_music_bc(x, outfile)
#'
#'   render_music_tab(x, outfile)
#'   render_music_guitar(x, outfile)
#'   render_music_bass(y, outfile)
#'
#'   # lyrics example
#'   render_music_guitar(z, outfile)
#' }
#' }
render_music <- function(music, file, clef = "treble", tab = FALSE,
                         tuning = "standard", string_names = NULL,
                         header = NULL, paper = NULL, midi = FALSE,
                         colors = NULL, transparent = FALSE, res = 150,
                         keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  lyrics <- .prep_lyrics(music)
  phrase(music) %>%
    track(clef = clef, tab = tab, tuning = tuning, lyrics = lyrics) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_tc <- function(music, file, header = NULL, paper = NULL,
                            midi = FALSE, colors = NULL, transparent = FALSE,
                            res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  lyrics <- .prep_lyrics(music)
  phrase(music) %>%
    track(clef = "treble", tab = FALSE, lyrics = lyrics) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, FALSE, TRUE, midi, colors,
        TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_bc <- function(music, file, header = NULL, paper = NULL,
                            midi = FALSE, colors = NULL, transparent = FALSE,
                            res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  lyrics <- .prep_lyrics(music)
  phrase(music) %>%
    track(clef = "bass", tab = FALSE, lyrics = lyrics) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, FALSE, TRUE, midi, colors,
        TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_tab <- function(music, file, clef = NA, tuning = "standard",
                             string_names = NULL, header = NULL, paper = NULL,
                             midi = FALSE, colors = NULL, transparent = FALSE,
                             res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  lyrics <- .prep_lyrics(music)
  phrase(music) %>%
    track(clef = clef, tuning = tuning, lyrics = lyrics) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_guitar <- function(music, file, tuning = "standard",
                                string_names = NULL, header = NULL,
                                paper = NULL, midi = FALSE,
                                colors = NULL, transparent = FALSE, res = 150,
                                keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  lyrics <- .prep_lyrics(music)
  phrase(music) %>%
    track(clef = "treble_8", tuning = tuning, lyrics = lyrics) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

#' @export
#' @rdname render_music
render_music_bass <- function(music, file, tuning = "bass",
                              string_names = NULL, header = NULL, paper = NULL,
                              midi = FALSE, colors = NULL, transparent = FALSE,
                              res = 150, keep_ly = FALSE, simplify = TRUE){
  ktt <- .ktt(music)
  paper <- .paper_snippet(paper)
  lyrics <- .prep_lyrics(music)
  phrase(music) %>%
    track(clef = "bass_8", tuning = tuning, lyrics = lyrics) %>%
    score() %>%
    tab(file, ktt[1], ktt[2], ktt[3], header, paper, string_names, TRUE, midi,
        colors, TRUE, transparent, res, keep_ly, simplify, FALSE)
}

.prep_lyrics <- function(x){
  y <- music_lyrics(x)
  if(all(is.na(y))) return(y)
  i1 <- which(note_is_rest(x))
  i2 <- grep("~", as_vector_time(x))
  if(length(i2)){
    i2 <- i2 + 1
    i2 <- i2[i2 %in% seq_along(x)]
    i1 <- unique(c(i1, i2))
  }
  if(length(i1)) y[-i1] else y
}

.paper_snippet <- function(x){
  if(is.null(x$fontsize)) x$fontsize <- 20
  if(is.null(x$page_numbers)) x$page_numbers <- FALSE
  x
}

.ktt <- function(x){
  c(music_key(x), music_time(x), music_tempo(x))
}

#' Plot sheet music snippet with LilyPond
#'
#' These functions are wrappers around the \code{render_music_*} functions.
#' They abstract the process of rendering a sheet music snippet to png and
#' loading the rendered image back into R to be displayed as a plot in an open
#' graphics device or inserted into an R markdown code chunk.
#'
#' While these functions abstract away the details of the process, this is not
#' the same as making the plot completely in R. R is only displaying the
#' intermediary png file. LilyPond is required to engrave the sheet music.
#'
#' For R markdown you can alternatively render the png using the corresponding
#' \code{render_music_*} function and then place it in the document explicitly
#' using \code{knitr::include_graphics}.
#' See \code{\link{render_music}} for more details.
#'
#' @param music a music object.
#' @param header a named list of arguments passed to the header of the
#' LilyPond file. See \code{lilypond} details.
#' @param paper a named list of arguments for the LilyPond file page layout.
#' See \code{lilypond} details.
#' @param clef character, include a music staff with the given clef.
#' \code{NA} to suppress. See \code{track} for details.
#' @param tab logical, include tablature staff. \code{NA} to suppress. See
#' \code{track}.
#' @param tuning character, string tuning, only applies to tablature. See
#' \code{track}.
#' @param string_names label strings at beginning of tab staff. \code{NULL}
#' (default) for non-standard tunings only, \code{TRUE} or \code{FALSE} for
#' force on or off completely.
#' @param colors a named list of LilyPond element color global overrides. See
#' \code{lilypond} for details.
#' @param transparent logical, transparent background for intermediate png file.
#' @param res numeric, resolution, png only. Defaults to 300.
#'
#' @return a plot
#' @export
#' @seealso \code{render_music}, \code{\link{phrase}}, \code{\link{track}},
#' \code{\link{score}}, \code{\link{lilypond}}, \code{\link{tab}}
#'
#' @examples
#' x <- "a,4;5*5 b,4- c4 cgc'e'~4 cgc'e'1 e'4;2 c';3 g;4 c;5 ce'1;51"
#' x <- as_music(x)
#'
#' y <- "a,,4;3*5 b,,4- c,4 c,g,c~4 c,g,c1 c4;1 g,;2 c,;3 g,;2 c,c1;31"
#' y <- as_music(y)
#'
#' \dontrun{
#' if(tabr_options()$lilypond != ""){ # requires LilyPond installation
#'   plot_music(x)
#'   plot_music(x, "treble_8", tab = TRUE)
#'
#'   plot_music_tc(x)
#'   plot_music_bc(x)
#'
#'   plot_music_tab(x)
#'   plot_music_guitar(x)
#'   plot_music_bass(y)
#' }
#' }
plot_music <- function(music, clef = "treble", tab = FALSE, tuning = "standard",
                       string_names = NULL, header = NULL, paper = NULL,
                       colors = NULL, transparent = FALSE, res = 300){
  file <- tempfile(fileext = ".png")
  render_music(music, file, clef, tab, tuning, string_names, header, paper,
               midi = FALSE, colors, transparent, res, keep_ly = FALSE,
               simplify = FALSE)
  .draw_image(file)
  unlink(file, recursive = TRUE, force = TRUE)
  invisible()
}

#' @export
#' @rdname plot_music
plot_music_tc <- function(music, header = NULL, paper = NULL, colors = NULL,
                          transparent = FALSE, res = 300){
  file <- tempfile(fileext = ".png")
  render_music_tc(music, file, header, paper, midi = FALSE, colors,
                  transparent, res, keep_ly = FALSE, simplify = FALSE)
  .draw_image(file)
  unlink(file, recursive = TRUE, force = TRUE)
  invisible()
}

#' @export
#' @rdname plot_music
plot_music_bc <- function(music, header = NULL, paper = NULL, colors = NULL,
                          transparent = FALSE, res = 300){
  file <- tempfile(fileext = ".png")
  render_music_bc(music, file, header, paper, midi = FALSE, colors,
                  transparent, res, keep_ly = FALSE, simplify = FALSE)
  .draw_image(file)
  unlink(file, recursive = TRUE, force = TRUE)
  invisible()
}

#' @export
#' @rdname plot_music
plot_music_tab <- function(music, clef = NA, tuning = "standard",
                           string_names = NULL, header = NULL, paper = NULL,
                           colors = NULL, transparent = FALSE, res = 300){
  file <- tempfile(fileext = ".png")
  render_music_tab(music, file, clef, tuning, string_names, header, paper,
                   midi = FALSE, colors, transparent, res, keep_ly = FALSE,
                   simplify = FALSE)
  .draw_image(file)
  unlink(file, recursive = TRUE, force = TRUE)
  invisible()
}

#' @export
#' @rdname plot_music
plot_music_guitar <- function(music, tuning = "standard", string_names = NULL,
                              header = NULL, paper = NULL, colors = NULL,
                              transparent = FALSE, res = 300){
  file <- tempfile(fileext = ".png")
  render_music_guitar(music, file, tuning, string_names, header, paper,
                      midi = FALSE, colors, transparent, res, keep_ly = FALSE,
                      simplify = FALSE)
  .draw_image(file)
  unlink(file, recursive = TRUE, force = TRUE)
  invisible()
}

#' @export
#' @rdname plot_music
plot_music_bass <- function(music, tuning = "bass", string_names = FALSE,
                            header = NULL, paper = NULL, colors = NULL,
                            transparent = FALSE, res = 300){
  file <- tempfile(fileext = ".png")
  render_music_bass(music, file, tuning = "bass", string_names, header, paper,
                    midi = FALSE, colors, transparent, res, keep_ly = FALSE,
                    simplify = FALSE)
  .draw_image(file)
  unlink(file, recursive = TRUE, force = TRUE)
  invisible()
}

#' @importFrom graphics par plot.new rasterImage
.draw_image <- function(file){
  if(!requireNamespace("png")){
    x <- "Please install the `png` package to read png files made by LilyPond."
    message(x)
    return(invisible())
  }
  img <- png::readPNG(file)
  op <- par(mar = rep(0, 4))
  plot.new()
  u <- par()$usr
  p <- par()$pin
  asp <- dim(img)[1] / dim(img)[2]
  width <- if(asp <= 1) 1 else 1 / asp
  height <- (u[4] - u[3]) / p[2]
  w <- width / (u[2] - u[1]) * p[1]
  h <- if(asp <= 1) w * asp * height else 1
  rasterImage(img, 0.5 - width / 2, 0.5 - h / 2, 0.5 + width / 2, 0.5 + h / 2,
              interpolate = TRUE)
  par(op)
}
